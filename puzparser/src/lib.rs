use Direction::*;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;

use encoding::DecoderTrap::Strict;
use encoding::Encoding;
use encoding::all::ISO_8859_1;

mod checksum;

use checksum::*;

// Loosely based on
// https://depth-first.com/articles/2021/12/16/a-beginners-guide-to-parsing-in-rust/
struct Scanner {
  cursor: usize,
  data: Vec<u8>,
}

impl Debug for Scanner {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Scanner")
      .field("cursor", &self.cursor)
      .finish()
  }
}

impl Scanner {
  fn new(data: Vec<u8>) -> Self {
    Self { cursor: 0, data }
  }

  /// Consume and return the next byte, or return None if that can't be done.
  fn pop(&mut self) -> Result<&u8, Error> {
    match self.data.get(self.cursor) {
      Some(byte) => {
        self.cursor += 1;
        Ok(byte)
      }
      None => Err(Error::EofError(self.cursor)),
    }
  }

  /// Consume the next two bytes and return them as a `u16`, interpreted as little-endian.
  fn parse_short(&mut self) -> Result<u16, Error> {
    if let Some(b1) = self.data.get(self.cursor) {
      if let Some(b2) = self.data.get(self.cursor + 1) {
        self.cursor += 2;
        return Ok(((*b2 as u16) << 8 | (*b1 as u16)).into());
      }
    }
    Err(Error::EofError(self.cursor))
  }

  /// Take the next `expected.len()` bytes, if they match `expected`.
  // TODO this probably needs a better name
  fn take_bytes(&mut self, expected: &[u8]) -> Result<(), Error> {
    for (i, expected_byte) in expected.iter().enumerate() {
      if let Some(b) = self.data.get(self.cursor + i) {
        if b != expected_byte {
          return Err(Error::ParseError(format!(
            "Expected byte 0x{:X} at position 0x{:X} but got 0x{:X}",
            expected_byte,
            self.cursor + i,
            b
          )));
        }
      }
    }
    self.cursor += expected.len();
    Ok(())
  }

  /// Take the next `n`` bytes.
  fn take_n_bytes(&mut self, n: usize) -> Result<Vec<u8>, Error> {
    if self.cursor >= self.data.len() {
      return Err(Error::EofError(self.cursor));
    }

    if self.cursor + n >= self.data.len() {
      return Err(Error::EofError(self.data.len()));
    }

    let data = &self.data[self.cursor..self.cursor + n];
    self.cursor += n;
    Ok(Vec::from(data))
  }

  /// Parses a C-style NUL-terminated string, including the NUL byte. In
  /// this function, we return just the raw bytes as they appear in the
  /// file. Converting them to a string (using the ISO-8859-1 encoding)
  /// is done later.
  fn parse_nul_terminated_string(&mut self) -> Result<Vec<u8>, Error> {
    for (index, byte) in self.data[self.cursor..].iter().enumerate() {
      if *byte == 0 {
        let bytes = &self.data[self.cursor..self.cursor + index + 1];
        self.cursor += index + 1;
        return Ok(bytes.to_vec());
      }
    }
    return Err(Error::EofError(self.data.len()));
  }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Direction {
  Across,
  Down,
}

#[derive(Debug)]
pub struct Puz {
  pub solution: Grid,
  pub solve_state: Grid,
  pub title: String,
  pub author: String,
  pub copyright: String,
  pub notes: String,
  pub numbered_squares: HashMap<u8, Pos>,
  pub clues: HashMap<(u8, Direction), String>,
}

impl Puz {
  /// Create a Puz from the bytes of a `.puz` file.
  pub fn parse(data: Vec<u8>) -> Result<(Self, Vec<ChecksumMismatch>), Error> {
    let mut checksum_mismatches = vec![];

    let cib_checksum_expected = checksum_region(&data[0x2C..0x34], 0);

    let mut scanner = Scanner::new(data);

    let overall_checksum = scanner.parse_short()?;
    scanner.take_bytes(b"ACROSS&DOWN\0")?;

    let cib_checksum = scanner.parse_short()?;
    if cib_checksum != cib_checksum_expected {
      checksum_mismatches.push(ChecksumMismatch {
        checksum: Checksum::CIB,
        expected: cib_checksum_expected,
        actual: cib_checksum,
      });
    }

    let masked_checksums = scanner.take_n_bytes(8)?;

    // Version string
    let _ = scanner.take_n_bytes(4)?;
    // Reserved 1C
    let _ = scanner.take_n_bytes(2)?;
    // Scrambled checksum
    let _ = scanner.take_n_bytes(2)?;

    // Nothing listed for these but it says the scrambled checksum ends at 0x1F and the
    // width should be at 0x2C so I guess we're just supposed to skip 0x20 through 0x2B?
    let _ = scanner.take_n_bytes(12)?;

    let width = *scanner.pop()? as usize;
    let height = *scanner.pop()? as usize;

    let num_clues = scanner.parse_short()?;

    // Unknown bitmask
    let _ = scanner.parse_short()?;

    let scrambled_tag = scanner.parse_short()?;
    if scrambled_tag != 0 {
      return Err(Error::ScrambledError);
    }

    let solution_bytes = scanner.take_n_bytes(width * height)?;
    let solution = Grid::parse(&solution_bytes, width, height);

    let solve_state_bytes = scanner.take_n_bytes(width * height)?;
    let solve_state = Grid::parse(&solve_state_bytes, width, height);

    let title = scanner.parse_nul_terminated_string()?;
    let author = scanner.parse_nul_terminated_string()?;
    let copyright = scanner.parse_nul_terminated_string()?;

    let mut clues = Vec::with_capacity(num_clues as usize);
    for _ in 0..num_clues {
      clues.push(scanner.parse_nul_terminated_string()?);
    }

    let notes = scanner.parse_nul_terminated_string()?;

    let overall_checksum_expected: u16 = {
      let mut c = checksum_region(&solution_bytes, cib_checksum);
      c = checksum_region(&solve_state_bytes, c);
      c = checksum_metadata_string(&title, c);
      c = checksum_metadata_string(&author, c);
      c = checksum_metadata_string(&copyright, c);
      for clue in clues.iter() {
        c = checksum_clue(&clue, c);
      }
      c = checksum_metadata_string(&notes, c);
      c
    };

    if overall_checksum != overall_checksum_expected {
      checksum_mismatches.push(ChecksumMismatch {
        checksum: Checksum::Overall,
        expected: overall_checksum_expected,
        actual: overall_checksum,
      })
    }

    let solution_checksum = checksum_region(&solution_bytes, 0);
    let grid_checksum = checksum_region(&solve_state_bytes, 0);
    let partial_board_checksum = {
      let mut c = 0;
      c = checksum_metadata_string(&title, c);
      c = checksum_metadata_string(&author, c);
      c = checksum_metadata_string(&copyright, c);
      for clue in clues.iter() {
        c = checksum_clue(clue, c);
      }
      c = checksum_metadata_string(&notes, c);
      c
    };

    let expected_masked_checksums = [
      0x49 ^ (cib_checksum & 0xFF) as u8,
      0x43 ^ (solution_checksum & 0xFF) as u8,
      0x48 ^ (grid_checksum & 0xFF) as u8,
      0x45 ^ (partial_board_checksum & 0xFF) as u8,
      0x41 ^ ((cib_checksum & 0xFF00) >> 8) as u8,
      0x54 ^ ((solution_checksum & 0xFF00) >> 8) as u8,
      0x45 ^ ((grid_checksum & 0xFF00) >> 8) as u8,
      0x44 ^ ((partial_board_checksum & 0xFF00) >> 8) as u8,
    ];

    assert_eq!(expected_masked_checksums.len(), masked_checksums.len());
    for (i, (expected, actual)) in expected_masked_checksums
      .iter()
      .zip(masked_checksums.iter())
      .enumerate()
    {
      if expected != actual {
        checksum_mismatches.push(ChecksumMismatch {
          checksum: Checksum::Masked(i),
          expected: *expected as u16,
          actual: *actual as u16,
        })
      }
    }

    let clues = clues
      .into_iter()
      .map(|clue| decode_str(&clue))
      .collect::<Result<Vec<String>, _>>()?;

    let (numbered_squares, clues) = allocate_clues(&solution, &clues);

    let puz = Self {
      solution,
      solve_state,
      title: decode_str(&title)?,
      author: decode_str(&author)?,
      copyright: decode_str(&copyright)?,
      notes: decode_str(&notes)?,
      numbered_squares,
      clues,
    };
    Ok((puz, checksum_mismatches))
  }
}

/// Turn a NUL-terminated ISO-8859-1-encoded string into a standard String.
fn decode_str(bytes: &[u8]) -> Result<String, Error> {
  assert_eq!(0x0, *bytes.last().unwrap());

  ISO_8859_1
    .decode(&bytes[0..bytes.len() - 1], Strict)
    .map_err(|e| {
      Error::Iso_8859_1Error(format!("Failed parsing '{:?}' as ISO-8859-1: {}", bytes, e))
    })
}

fn allocate_clues(
  grid: &Grid,
  clue_list: &[String],
) -> (HashMap<u8, Pos>, HashMap<(u8, Direction), String>) {
  let mut clue_number: u8 = 1;
  let mut numbered_squares = HashMap::new();
  let mut clues = HashMap::with_capacity(clue_list.len());

  let mut clue_iter = clue_list.into_iter();

  for pos in grid.positions() {
    let starts_across = grid.starts_across(pos);
    let starts_down = grid.starts_down(pos);

    if starts_across || starts_down {
      numbered_squares.insert(clue_number, pos);

      if starts_across {
        let clue = clue_iter.next().unwrap();
        clues.insert((clue_number, Across), clue.clone());
      }

      if starts_down {
        let clue = clue_iter.next().unwrap();
        clues.insert((clue_number, Down), clue.clone());
      }

      clue_number += 1;
    }
  }

  (numbered_squares, clues)
}

/// A square in a crossword grid.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Square {
  Black,
  Empty,
  Letter(char),
}

impl Square {
  /// Is this a black square (not to be confused with a blank (white) square)?
  fn is_black(&self) -> bool {
    *self == Self::Black
  }

  /// Is this a white square? A white square may currently have a letter or it may be blank.
  fn is_white(&self) -> bool {
    !self.is_black()
  }
}

impl Debug for Square {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Black => write!(f, "■"),
      Self::Empty => write!(f, " "),
      Self::Letter(c) => write!(f, "{}", c),
    }?;
    Ok(())
  }
}

impl Display for Square {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

impl From<&u8> for Square {
  fn from(value: &u8) -> Self {
    if *value == b'.' {
      Self::Black
    } else if *value == b'-' {
      Self::Empty
    } else {
      Self::Letter(*value as char)
    }
  }
}

/// A position in a grid: (row, column)
type Pos = (usize, usize);

#[derive(Debug, Eq, PartialEq)]
pub struct Grid(Vec<Vec<Square>>);

impl Grid {
  /// Create a new grid from the given bytes.
  fn parse(bytes: &[u8], width: usize, height: usize) -> Self {
    assert_eq!(bytes.len(), width * height);

    let mut grid = Vec::with_capacity(height);

    for chunk in bytes.chunks(width) {
      let row = chunk.iter().map(|b| b.into()).collect::<Vec<Square>>();
      grid.push(row);
    }

    Self(grid)
  }

  /// The size of this grid, expressed as (width, height).
  fn size(&self) -> (usize, usize) {
    (self.0[0].len(), self.0.len())
  }

  /// An iterator over all the positions of this grid, from left to right and top to bottom.
  fn positions(&self) -> GridPosIter {
    GridPosIter::new(self.size())
  }

  fn get(&self, (r, c): Pos) -> Square {
    self.0[r][c]
  }

  /// Returns an iterator over all the squares in the grid,
  /// paired with their position
  pub fn enumerate(&self) -> impl Iterator<Item = (Pos, Square)> {
    GridIter {
      pos_iter: self.positions(),
      grid: self,
    }
  }

  /// Returns an iterator over all the white squares in the grid,
  /// paired with their position
  pub fn enumerate_white(&self) -> impl Iterator<Item = (Pos, Square)> {
    self.enumerate().filter(|(_, sq)| sq.is_white())
  }

  /// Whether the given position is the start of an Across entry.
  fn starts_across(&self, (row, col): Pos) -> bool {
    if self.get((row, col)).is_black() {
      return false;
    }

    let (width, _) = self.size();

    if (col == 0 || self.get((row, col - 1)).is_black())
      && col != width - 1
      && self.get((row, col + 1)).is_white()
    {
      // Start of an "across" word.
      return true;
    }

    return false;
  }

  fn starts_down(&self, (row, col): Pos) -> bool {
    if self.get((row, col)).is_black() {
      return false;
    }

    let (_, height) = self.size();

    if (row == 0 || self.get((row - 1, col)).is_black())
      && row != height - 1
      && self.get((row + 1, col)).is_white()
    {
      return true;
    }
    return false;
  }
}

/// Iterator over all the positions in the grid.
struct GridPosIter {
  pos: (usize, usize),
  size: (usize, usize),
}
impl GridPosIter {
  fn new(size: (usize, usize)) -> Self {
    Self { pos: (0, 0), size }
  }
}

impl Iterator for GridPosIter {
  type Item = Pos;
  fn next(&mut self) -> Option<Self::Item> {
    let (width, height) = self.size;
    let (row, col) = self.pos;

    if row == height {
      return None;
    }

    if col == width - 1 {
      self.pos = (row + 1, 0);
    } else {
      self.pos = (row, col + 1);
    }

    Some((row, col))
  }
}

struct GridIter<'a> {
  grid: &'a Grid,
  pos_iter: GridPosIter,
}

impl Iterator for GridIter<'_> {
  type Item = (Pos, Square);
  fn next(&mut self) -> Option<Self::Item> {
    let pos = self.pos_iter.next()?;
    Some((pos, self.grid.get(pos)))
  }
}

impl Display for Grid {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for row in &self.0 {
      for sq in row {
        write!(f, "{}", sq)?;
      }
      writeln!(f)?;
    }
    Ok(())
  }
}

#[derive(Debug)]
pub enum Error {
  // Unexpectedly reached the end of the file.
  EofError(usize),
  ParseError(String),
  #[allow(non_camel_case_types)]
  Iso_8859_1Error(String),
  ScrambledError,
  IoError(std::io::Error),
}

#[derive(Eq, PartialEq)]
pub struct ChecksumMismatch {
  checksum: Checksum,
  expected: u16,
  actual: u16,
}

#[derive(Debug, Eq, PartialEq)]
enum Checksum {
  CIB,
  Overall,
  Masked(usize),
}

impl Display for Checksum {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self)?;
    Ok(())
  }
}

impl Debug for ChecksumMismatch {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "Mismatch on checksum {}: Expected {:#x} but got {:#x}",
      self.checksum, self.expected, self.actual
    )
  }
}

impl From<std::io::Error> for Error {
  fn from(e: std::io::Error) -> Self {
    Self::IoError(e)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  #[test]
  fn parse_v12_puzzle() {
    let data: Vec<u8> = fs::read("puzzles/version-1.2-puzzle.puz").unwrap();
    let (puz, checksum_mismatches) = Puz::parse(data).unwrap();
    assert_eq!(checksum_mismatches, []);
    assert_eq!(puz.title, "Reference PUZ File");
    assert_eq!(puz.author, "Josh Myer");
    assert_eq!(puz.copyright, "Copyright (c) 2005 Josh Myer");
    assert_eq!(puz.notes, "");

    #[rustfmt::skip]
    assert_eq!(puz.numbered_squares, HashMap::from([
      (1, (0, 1)),
      (2, (1, 0)),
      (3, (1, 3)),
      (4, (3, 1)),
    ]));

    #[rustfmt::skip]
    assert_eq!(
      puz.clues,
      HashMap::from([
        ((1, Down), "Pumps your basement".into()), // SUMP
        ((2, Across), "I'm ___, thanks for asking\\!".into()), // SUPER
        ((3, Down), "Until".into()), // ERE
        ((4, Across), "One step short of a pier".into()), // PIE
      ])
    );

    #[rustfmt::skip]
    assert_eq!(
      puz.solution.to_string(),
      concat!(
        "■S■■■\n",
        "SUPER\n",
        "■M■R■\n",
        "■PIE■\n",
      )
    );

    #[rustfmt::skip]
    assert_eq!(
      puz.solve_state.to_string(),
      concat!(
        "■S■■■\n",
        " U   \n",
        "■M■ ■\n",
        "■P  ■\n",
      )
    );
  }
}
