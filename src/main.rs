use std::env;
use std::fmt::Debug;
use std::fmt::Display;
use std::fs;
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

  /// Return the next byte without advancing the cursor.
  fn peek(&self) -> Result<&u8, Error> {
    self
      .data
      .get(self.cursor)
      .ok_or(Error::EofError(self.cursor))
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

  /// Return all the bytes from the current cursor up to the next NUL byte.
  /// Note that the cursor is moved to the next byte after the NUL, but the NUL
  /// is not included in the returned string.
  fn parse_nul_terminated_string(&mut self) -> Result<String, Error> {
    for (index, byte) in self.data[self.cursor..].iter().enumerate() {
      if *byte == 0x0 {
        let bytes = &self.data[self.cursor..self.cursor + index];
        self.cursor += index + 1;
        return Ok(String::from_utf8(bytes.to_vec())?);
      }
    }
    Err(Error::EofError(self.data.len()))
  }
}

#[derive(Debug)]
#[allow(unused)]
struct Puz {
  version: String,
  width: usize,
  height: usize,
  solution: Grid,
  solve_state: Grid,
  title: String,
  author: String,
  copyright: String,
  clues: Vec<String>,
  notes: String,
}

/// A square in a crossword grid. None represents a black square.
// TODO: I think this should actually be an enum, something like
// enum Square {
//   Black,
//   Letter(char, Option<u8>)
// }
// to indicate both the letter that's on it and the number. Then
// in the solve state we can either use ' ' for blank or change the char to an Option<char>
struct Square(Option<char>);

impl Debug for Square {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self.0 {
      None => write!(f, "■"),
      Some(c) => write!(f, "{}", c),
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
      Self(None)
    } else if *value == b'-' {
      Self(Some(' '))
    } else {
      Self(Some(*value as char))
    }
  }
}

#[derive(Debug)]
struct Grid(Vec<Vec<Square>>);

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
enum Error {
  // Unexpectedly reached the end of the file.
  EofError(usize),
  ParseError(String),
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

#[allow(unused)] // for now since there are a lot of checksum-related variables not being used.
fn parse_puz_file(data: Vec<u8>) -> Result<Puz, Error> {
  let mut scanner = Scanner::new(data);

  let checksum = scanner.parse_short()?;
  scanner.take_bytes(b"ACROSS&DOWN\0")?;

  let cib_checksum = scanner.parse_short()?;
  let masked_low_checksums = scanner.take_n_bytes(4)?;
  let masked_high_checksums = scanner.take_n_bytes(4)?;

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
  let solution = parse_grid(&solution_bytes, width, height);

  let solve_state_bytes = scanner.take_n_bytes(width * height)?;
  let solve_state = parse_grid(&solve_state_bytes, width, height);

  let title = scanner.parse_nul_terminated_string()?;
  let author = scanner.parse_nul_terminated_string()?;
  let copyright = scanner.parse_nul_terminated_string()?;

  let mut clues = Vec::with_capacity(num_clues as usize);
  for _ in 0..num_clues {
    clues.push(scanner.parse_nul_terminated_string()?);
  }

  let notes = scanner.parse_nul_terminated_string()?;

  // Next: Need to check the checksums and assign the clues to the correct rows and columns.

  Ok(Puz {
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

fn parse_grid(solution_bytes: &[u8], width: usize, height: usize) -> Grid {
  let mut grid = Vec::with_capacity(height);

  for chunk in solution_bytes.chunks(width) {
    let row = chunk.iter().map(|b| b.into()).collect::<Vec<Square>>();
    grid.push(row);
  }

  Grid(grid)
}

fn main() -> Result<(), Error> {
  let args: Vec<String> = env::args().collect();

  let data: Vec<u8> = fs::read(&args[1])?;
  let puz = parse_puz_file(data)?;
  println!("{:?}", puz);
  Ok(())
}
