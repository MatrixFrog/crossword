use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;

use encoding::DecoderTrap::Strict;
use encoding::Encoding;
use encoding::all::ISO_8859_1;

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

  /// Take the next n bytes, or
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

#[derive(Debug)]
pub struct Puz {
  pub width: usize,
  pub height: usize,
  pub solution: Grid,
  pub solve_state: Grid,
  pub title: String,
  pub author: String,
  pub copyright: String,
  pub clues: Vec<String>,
  pub notes: String,
}

impl Puz {
  pub fn parse_ignoring_checksums(data: Vec<u8>) -> Result<Self, Error> {
    Self::parse(data, false)
  }

  /// Create a Puz from the bytes of a `.puz` file.
  fn parse(data: Vec<u8>, verify_checksums: bool) -> Result<Self, Error> {
    let cib_checksum_expected = Self::checksum(&data[0x2C..0x34], 0);

    let mut scanner = Scanner::new(data);

    let overall_checksum = scanner.parse_short()?;
    scanner.take_bytes(b"ACROSS&DOWN\0")?;

    let cib_checksum = scanner.parse_short()?;
    if verify_checksums && cib_checksum != cib_checksum_expected {
      return Err(Error::ChecksumError("CIB".into()));
    }

    let masked_checksums = scanner.take_n_bytes(8)?;

    // Version string
    let _ = scanner.take_n_bytes(4)?.to_vec();
    // Reserved 1C
    let _ = scanner.take_n_bytes(2)?;

    let scrambled_checksum = scanner.take_n_bytes(2)?;

    // Nothing listed for these but it says the scrambled checksum ends at 0x1F and the
    // width should be at 0x2C so I guess we're just supposed to skip 0x20 through 0x2B?
    let _ = scanner.take_n_bytes(12)?;

    let width = *scanner.pop()? as usize;
    let height = *scanner.pop()? as usize;

    let num_clues = scanner.parse_short()?;

    // Unknown bitmask
    let _ = scanner.parse_short()?;

    // Scrambled tag
    let _ = scanner.parse_short()?;

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

    // dbg!(&title, &author, &copyright, &notes);

    // TODO: Store these as private data on the Puz object somehow.
    let numbered_squares = solution
      .positions()
      .filter(|&pos| solution.need_number_at(pos))
      .enumerate()
      .map(|(n, pos)| (n + 1, pos)) // Clue numbers start at 1, not 0.
      .collect::<HashMap<usize, Pos>>();

    if verify_checksums {
      let overall_checksum_expected: u16 = {
        let mut c = Self::checksum(&solution_bytes, cib_checksum);
        c = Self::checksum(&solve_state_bytes, c);
        c = Self::checksum_metadata_string(&title, c);
        c = Self::checksum_metadata_string(&author, c);
        c = Self::checksum_metadata_string(&copyright, c);
        for clue in clues.iter() {
          Self::checksum_clue(&clue, c);
        }
        c = Self::checksum_metadata_string(&notes, c);
        c
      };

      if overall_checksum != overall_checksum_expected {
        return Err(Error::ChecksumError(format!(
          "Overall checksum: Expected {} but got {}",
          overall_checksum_expected, overall_checksum,
        )));
      }

      let solution_checksum = Self::checksum(&solution_bytes, 0);
      let grid_checksum = Self::checksum(&solve_state_bytes, 0);
      let mut partial_board_checksum = 0;
      partial_board_checksum = Self::checksum_metadata_string(&title, partial_board_checksum);
      partial_board_checksum = Self::checksum_metadata_string(&author, partial_board_checksum);
      partial_board_checksum = Self::checksum_metadata_string(&copyright, partial_board_checksum);
      for clue in clues.iter() {
        partial_board_checksum = Self::checksum_clue(clue, partial_board_checksum);
      }
      partial_board_checksum = Self::checksum_metadata_string(&notes, partial_board_checksum);

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

      if masked_checksums != expected_masked_checksums {
        return Err(Error::ChecksumError(format!(
          "Masked checksums mismatch: Expected {:?} but got {:?}",
          expected_masked_checksums, masked_checksums
        )));
      }
    }

    Ok(Self {
      width,
      height,
      solution,
      solve_state,
      title: Self::decode_str(&title)?,
      author: Self::decode_str(&author)?,
      copyright: Self::decode_str(&copyright)?,
      clues: clues
        .into_iter()
        .map(|clue| Self::decode_str(&clue))
        .collect::<Result<Vec<_>, Error>>()?,
      notes: Self::decode_str(&notes)?,
    })
  }

  fn decode_str(bytes: &[u8]) -> Result<String, Error> {
    assert_eq!(0x0, *bytes.last().unwrap());

    ISO_8859_1
      .decode(&bytes[0..bytes.len() - 1], Strict)
      .map_err(|e| {
        Error::Iso_8859_1Error(format!("Failed parsing '{:?}' as ISO-8859-1: {}", bytes, e))
      })
  }

  // https://gist.github.com/sliminality/dab21fa834eae0a70193c7cd69c356d5#checksums
  fn checksum(base: &[u8], input_checksum: u16) -> u16 {
    let mut checksum = input_checksum;
    for &byte in base {
      if checksum & 0x0001_u16 != 0 {
        checksum = (checksum >> 1) + 0x8000
      } else {
        checksum = checksum >> 1;
      }
      checksum = checksum.overflowing_add(byte as u16).0;
    }
    checksum
  }

  /// Part of the checksum calculations. For metadata (title, author, copyright, or notes),
  /// we do nothing if the string is empty, but if it's not empty we include the \0 byte in
  /// the calculation.
  fn checksum_metadata_string(s: &[u8], input_checksum: u16) -> u16 {
    if s == b"\0" {
      return input_checksum;
    }

    return Self::checksum(s, input_checksum);
  }

  /// Part of the checksum calculations. For clues, we do not include the trailing \0 byte.
  fn checksum_clue(s: &[u8], input_checksum: u16) -> u16 {
    Self::checksum(&s[0..s.len() - 1], input_checksum)
  }
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

#[derive(Debug)]
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

  /// Whether there should be a number at the given position, because it
  /// is the start of a word (either across or down).
  fn need_number_at(&self, (row, col): Pos) -> bool {
    if self.get((row, col)).is_black() {
      return false;
    }

    let (width, height) = self.size();

    if (row == 0 || self.get((row - 1, col)).is_black())
      && row != height - 1
      && self.get((row + 1, col)).is_white()
    {
      // Start of a "down" word.
      return true;
    }

    if (col == 0 || self.get((row, col - 1)).is_black())
      && col != width - 1
      && self.get((row, col + 1)).is_white()
    {
      // Start of an "across" word.
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
#[allow(unused)]
pub enum Error {
  // Unexpectedly reached the end of the file.
  EofError(usize),
  ParseError(String),
  ChecksumError(String),
  #[allow(non_camel_case_types)]
  Iso_8859_1Error(String),
  IoError(std::io::Error),
}

impl From<std::io::Error> for Error {
  fn from(e: std::io::Error) -> Self {
    Self::IoError(e)
  }
}
