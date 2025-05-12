//! This crate is meant to be used as the foundation for a crossword puzzle app.
//! It provides no UI itself, but see `crosstui` for an example of how you can use it
//! to produce a crossword app.
//!
//! Puzzles are loaded from `.puz` files, a de facto standard format for crossword puzzles.
//! You can find `.puz` files to download on many crossword sites.

use Direction::{Across, Down};
use std::cmp::{max, min};
use std::fmt::Debug;
use std::fmt::Display;

mod checksum;
mod puz;

pub use puz::{ChecksumMismatch, Puz};

/// The two crossword directions: `Across` and `Down`
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum Direction {
  Across,
  Down,
}

/// Represents a crossword puzzle and a Cursor. When implementing a crossword app,
/// this will be the main structure you will use.
#[derive(Debug)]
pub struct Puzzle {
  puz: Puz,
  cursor: Cursor,
}

impl Puzzle {
  /// Creates a Puzzle from the bytes of a `.puz` file.
  pub fn parse(data: Vec<u8>) -> Result<(Self, Vec<ChecksumMismatch>), Error> {
    let (puz, checksum_mismatches) = Puz::parse(data)?;
    let cursor = Cursor::from_grid(&puz.solve_state);
    let puzzle = Self { puz, cursor };
    Ok((puzzle, checksum_mismatches))
  }

  /// Whether the puzzle is fully filled in, and matches the solution.
  pub fn is_solved(&self) -> bool {
    self.grid().is_filled() && *self.grid() == self.puz.solution
  }

  /// Returns a reference to the current puzzle grid.
  pub fn grid(&self) -> &Grid {
    &self.puz.solve_state
  }

  pub fn title(&self) -> &str {
    &self.puz.title
  }

  /// Determines how a particular square should be styled.
  /// See [SquareStyle].
  pub fn square_style(&self, pos: Pos) -> SquareStyle {
    if pos == self.cursor.pos {
      return SquareStyle::Cursor;
    }

    let (row, col) = pos;
    let (cursor_row, cursor_col) = self.cursor.pos;

    if self.cursor.direction == Across && row == cursor_row {
      let (col_start, col_end) = (min(col, cursor_col), max(col, cursor_col));
      if (col_start..col_end).any(|c| self.grid().get((row, c)).is_black()) {
        return SquareStyle::Standard;
      } else {
        return SquareStyle::Word;
      }
    }

    if self.cursor.direction == Down && col == cursor_col {
      let (row_start, row_end) = (min(row, cursor_row), max(row, cursor_row));
      if (row_start..row_end).any(|r| self.grid().get((r, col)).is_black()) {
        return SquareStyle::Standard;
      } else {
        return SquareStyle::Word;
      }
    }

    SquareStyle::Standard
  }

  /// Returns the text of the clue corresponding to the [Cursor].
  pub fn current_clue(&self) -> &str {
    let pos = self.puz.solve_state.get_start(&self.cursor);
    let clue_number = *self.puz.numbered_squares.get(&pos).unwrap();
    self
      .puz
      .clues
      .get(&(clue_number, self.cursor.direction))
      .unwrap()
  }

  /// Writes the given letter to the current square, and advances the cursor.
  pub fn add_letter(&mut self, letter: char) {
    assert!(letter.is_ascii_alphabetic());

    self
      .puz
      .solve_state
      .set(self.cursor.pos, Square::Letter(letter.to_ascii_uppercase()));
    self.cursor.advance(&self.puz.solve_state);
  }

  /// Sets the current square to [Empty](Square::Empty) and moves the cursor back a square.
  pub fn delete_square(&mut self) {
    self.puz.solve_state.set(self.cursor.pos, Square::Empty);
    self.cursor.backup(&self.puz.solve_state);
  }

  /// Attempts to swap the cursor direction. However, if the current square is
  /// only part of an across clue, the direction cannot be switched to down,
  /// and vice versa.
  pub fn swap_cursor_direction(&mut self) {
    self.cursor.direction = match self.cursor.direction {
      Across => Down,
      Down => Across,
    };
    self.cursor.adjust_direction(&self.puz.solve_state);
  }

  pub fn cursor_up(&mut self) {
    self.cursor.up(&self.puz.solve_state);
  }
  pub fn cursor_down(&mut self) {
    self.cursor.down(&self.puz.solve_state);
  }
  pub fn cursor_left(&mut self) {
    self.cursor.left(&self.puz.solve_state);
  }
  pub fn cursor_right(&mut self) {
    self.cursor.right(&self.puz.solve_state);
  }
}

/// The type returned from [Puzzle::square_style].
#[derive(Debug)]
pub enum SquareStyle {
  /// Default styling
  Standard,
  /// The cursor is positioned on this square.
  Cursor,
  /// The cursor is not on this square, but the word indicated by the cursor includes this square.
  Word,
}

/// A square in a crossword grid.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Square {
  /// A black square where nothing can be entered.
  Black,
  /// A square where a letter could be entered, but that is currently empty.
  Empty,
  /// A square with a letter written in it.
  Letter(char),
}

impl Square {
  /// Whether this is [Square::Black].
  pub fn is_black(&self) -> bool {
    *self == Self::Black
  }

  /// Whether this is not a black square, i.e. either a [Square::Empty] or [Square::Letter].
  pub fn is_white(&self) -> bool {
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
pub type Pos = (usize, usize);

/// A grid of squares. Used to represent the current state of a partially-solved puzzle,
/// or the solution of a puzzle.
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
    (self.width(), self.height())
  }

  /// The width of this grid.
  pub fn width(&self) -> usize {
    self.0[0].len()
  }

  /// The height of this grid.
  pub fn height(&self) -> usize {
    self.0.len()
  }

  /// An iterator over all the positions of this grid, from left to right and top to bottom.
  fn positions(&self) -> GridPosIter {
    GridPosIter::new(self.size())
  }

  /// Whether this grid is fully filled in -- that is, has no `Square::Empty` in it.
  fn is_filled(&self) -> bool {
    !self.0.iter().flatten().any(|&sq| sq == Square::Empty)
  }

  /// Returns the [Square] at the given [Pos].
  pub fn get(&self, (r, c): Pos) -> Square {
    self.0[r][c]
  }

  fn set(&mut self, (r, c): Pos, square: Square) {
    self.0[r][c] = square;
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

  /// Returns the square immediately to the left of the given position, or
  /// `Square::Black` if the given position is on the left edge of the grid.
  fn left_neighbor(&self, (row, col): Pos) -> Square {
    if col == 0 {
      Square::Black
    } else {
      self.get((row, col - 1))
    }
  }

  /// Returns the square immediately to the right of the given position, or
  /// `Square::Black` if the given position is on the right edge of the grid.
  fn right_neighbor(&self, (row, col): Pos) -> Square {
    if col + 1 == self.width() {
      Square::Black
    } else {
      self.get((row, col + 1))
    }
  }

  /// Returns the square immediately above the given position, or
  /// `Square::Black` if the given position is on the top edge of the grid.
  fn up_neighbor(&self, (row, col): Pos) -> Square {
    if row == 0 {
      Square::Black
    } else {
      self.get((row - 1, col))
    }
  }

  /// Returns the square immediately below the given position, or
  /// `Square::Black` if the given position is on the bottom edge of the grid.
  fn down_neighbor(&self, (row, col): Pos) -> Square {
    if row + 1 == self.height() {
      Square::Black
    } else {
      self.get((row + 1, col))
    }
  }

  /// Whether the given position is the start of an Across entry.
  fn starts_across(&self, pos: Pos) -> bool {
    if self.get(pos).is_black() {
      return false;
    }

    self.left_neighbor(pos).is_black() && self.right_neighbor(pos).is_white()
  }

  fn starts_down(&self, pos: Pos) -> bool {
    if self.get(pos).is_black() {
      return false;
    }

    self.up_neighbor(pos).is_black() && self.down_neighbor(pos).is_white()
  }

  // Given a cursor, determine the position of the start of the word that contains that cursor.
  pub fn get_start(&self, cursor: &Cursor) -> Pos {
    let mut pos = cursor.pos;
    match cursor.direction {
      Across => loop {
        if self.starts_across(pos) {
          return pos;
        }
        let (row, col) = pos;
        pos = (row, col - 1);
      },
      Down => loop {
        if self.starts_down(pos) {
          return pos;
        }
        let (row, col) = pos;
        pos = (row - 1, col);
      },
    }
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

/// Represents the position of the user's currently-highlighted square, and the `Direction`
/// of the word they are currently entering.
#[derive(Debug)]
pub struct Cursor {
  /// The position of the currently-highlighted square.
  pub pos: Pos,
  /// The current direction.
  pub direction: Direction,
}

impl Cursor {
  pub fn from_grid(grid: &Grid) -> Self {
    let (pos, _) = grid.enumerate_white().next().unwrap();

    let mut cursor = Self {
      pos,
      direction: Across,
    };

    cursor.adjust_direction(grid);

    cursor
  }

  fn adjust_direction(&mut self, grid: &Grid) {
    match self.direction {
      Across => {
        if grid.right_neighbor(self.pos).is_black() && grid.left_neighbor(self.pos).is_black() {
          self.direction = Down;
        }
      }
      Down => {
        if grid.up_neighbor(self.pos).is_black() && grid.down_neighbor(self.pos).is_black() {
          self.direction = Across;
        }
      }
    }
  }

  fn advance(&mut self, grid: &Grid) {
    match self.direction {
      Across => self.right(grid),
      Down => self.down(grid),
    }
  }

  fn backup(&mut self, grid: &Grid) {
    match self.direction {
      Across => self.left(grid),
      Down => self.up(grid),
    }
  }

  pub fn up(&mut self, grid: &Grid) {
    if grid.up_neighbor(self.pos).is_black() {
      return;
    }

    let (row, col) = self.pos;
    self.pos = (row - 1, col);
    self.adjust_direction(grid);
  }

  pub fn down(&mut self, grid: &Grid) {
    if grid.down_neighbor(self.pos).is_black() {
      return;
    }

    let (row, col) = self.pos;
    self.pos = (row + 1, col);
    self.adjust_direction(grid);
  }

  pub fn left(&mut self, grid: &Grid) {
    if grid.left_neighbor(self.pos).is_black() {
      return;
    }

    let (row, col) = self.pos;
    self.pos = (row, col - 1);
    self.adjust_direction(grid);
  }

  pub fn right(&mut self, grid: &Grid) {
    if grid.right_neighbor(self.pos).is_black() {
      return;
    }

    let (row, col) = self.pos;
    self.pos = (row, col + 1);
    self.adjust_direction(grid);
  }
}

/// The errors that may be produced by functions in this crate.
#[derive(Debug)]
pub enum Error {
  /// Unexpectedly reached the end of the file at the given byte index.
  EofError(usize),
  /// Something went wrong while parsing a .puz file.
  ParseError(String),
  /// Got an error while decoding a string, possibly because it was incorrectly
  /// encoding using another encoding instead of ISO-8859-1.
  #[allow(non_camel_case_types)]
  Iso_8859_1Error(String),
  /// The given puz file was marked as "scrambled" which this crate doesn't support.
  ScrambledError,
  /// An [I/O error](std::io::Error) occurred.
  IoError(std::io::Error),
}

impl From<std::io::Error> for Error {
  fn from(e: std::io::Error) -> Self {
    Self::IoError(e)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::{collections::HashMap, fs};

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
      ((0, 1), 1),
      ((1, 0), 2),
      ((1, 3), 3),
      ((3, 1), 4),
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
