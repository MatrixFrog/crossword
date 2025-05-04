use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::string::FromUtf8Error;

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

  /// Return all the bytes from the current cursor up to and including the next NUL byte.
  fn parse_nul_terminated_string(&mut self) -> Result<String, Error> {
    for (index, byte) in self.data[self.cursor..].iter().enumerate() {
      if *byte == 0 {
        let bytes = &self.data[self.cursor..self.cursor + index + 1];
        self.cursor += index + 1;
        return Ok(String::from_utf8(bytes.to_vec())?);
      }
    }
    Err(Error::EofError(self.data.len()))
  }
}

#[derive(Debug)]
pub struct Puz {
  pub version: String,
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
  /// Create a Puz struct from the bytes of a `.puz` file.
  #[allow(unused)] // for now since there are a lot of checksum-related variables not being used yet.
  pub fn parse(data: Vec<u8>) -> Result<Self, Error> {
    let cib_checksum_expected = Self::checksum(&data[0x2C..0x34], 0);

    let mut scanner = Scanner::new(data);

    let checksum = scanner.parse_short()?;
    scanner.take_bytes(b"ACROSS&DOWN\0")?;

    let cib_checksum = scanner.parse_short()?;
    if cib_checksum != cib_checksum_expected {
      return Err(Error::ChecksumError("CIB".into()));
    }

    let masked_checksums = scanner.take_n_bytes(8)?;

    // Version string
    let version = String::from_utf8(scanner.take_n_bytes(4)?.to_vec())?;
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

    // TODO: Store these as private data on the Puz object somehow.
    let numbered_squares = solution
      .positions()
      .filter(|&pos| solution.need_number_at(pos))
      .enumerate()
      .map(|(n, pos)| (n + 1, pos)) // Clue numbers start at 1, not 0.
      .collect::<HashMap<usize, Pos>>();

    let solution_checksum = Self::checksum(&solution_bytes, 0);
    let grid_checksum = Self::checksum(&solve_state_bytes, 0);
    let mut partial_board_checksum = Self::checksum(title.as_bytes(), 0);
    partial_board_checksum = Self::checksum(author.as_bytes(), partial_board_checksum);
    partial_board_checksum = Self::checksum(copyright.as_bytes(), partial_board_checksum);
    for clue in clues.iter() {
      // For the clues, don't include the \0 in the calculation.
      let clue_bytes = clue.as_bytes();
      let clue_bytes = &clue_bytes[0..clue_bytes.len() - 1];
      partial_board_checksum = Self::checksum(&clue_bytes, partial_board_checksum);
    }
    partial_board_checksum = Self::checksum(notes.as_bytes(), partial_board_checksum);

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
      assert_eq!(masked_checksums, expected_masked_checksums);
      return Err(Error::ChecksumError("Masked checksums".into()));
    }

    // TODO strip the \0 bytes off of the title, author, clues, etc.
    Ok(Self {
      version,
      width,
      height,
      solution,
      solve_state,
      title,
      author,
      copyright,
      clues,
      notes,
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

    if (row == 0 || self.get((row - 1, col)).is_black()) && (self.get((row + 1, col)).is_white()) {
      // Start of a "down" word.
      return true;
    }

    if (col == 0 || self.get((row, col - 1)).is_black()) && (self.get((row, col + 1)).is_white()) {
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
  Utf8Error,
  IoError(std::io::Error),
}

impl From<std::io::Error> for Error {
  fn from(e: std::io::Error) -> Self {
    Self::IoError(e)
  }
}

impl From<FromUtf8Error> for Error {
  fn from(_: FromUtf8Error) -> Self {
    Self::Utf8Error
  }
}
