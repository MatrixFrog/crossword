use std::cmp::{max, min};
use std::{env, io};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use puzparser::Direction::{Across, Down};
use puzparser::{Cursor, Pos, Puz, Square};
use ratatui::{
  DefaultTerminal, Frame,
  buffer::Buffer,
  layout::{Constraint, Flex, Layout, Rect},
  style::{Color, Modifier, Style, Stylize},
  text::Line,
  widgets::{Block, Padding, Paragraph, Widget},
};

const SQUARE_WIDTH: u16 = 7;
const SQUARE_HEIGHT: u16 = 3;

fn main() -> io::Result<()> {
  let args: Vec<String> = env::args().collect();

  if args.len() < 2 {
    println!("Pass the filename of a .puz file to play");
    std::process::exit(1);
  }

  let puz = parse_puz(&args[1]);
  let app = App::new(puz);

  let terminal = ratatui::init();
  let result = app.run(terminal);
  ratatui::restore();
  result
}

pub fn parse_puz(path: &str) -> Puz {
  let data: Vec<u8> = std::fs::read(path).unwrap_or_else(|err| {
    println!("{:?}", err);
    std::process::exit(1);
  });
  let (puzzle, checksum_mismatches) = Puz::parse(data).unwrap_or_else(|e| {
    println!("Failed to parse .puz file: {:?}", e);
    std::process::exit(2);
  });

  if !checksum_mismatches.is_empty() {
    println!(
      ".puz file parsing encountered checksum mismatches: {:?}",
      checksum_mismatches
    );
    std::process::exit(3);
  }
  puzzle
}

#[derive(Debug)]
enum SquareStyle {
  // Default styling
  Standard,
  // The cursor is positioned on this square.
  Cursor,
  // This cursor is not on this square, but the word indicated by the cursor includes this square.
  Word,
}

impl From<SquareStyle> for Style {
  fn from(value: SquareStyle) -> Self {
    let base_style = match value {
      SquareStyle::Standard => Style::new().bg(Color::White),
      SquareStyle::Cursor => Style::new().bg(Color::LightRed),
      SquareStyle::Word => Style::new().bg(Color::LightYellow),
    };
    base_style.fg(Color::Black).add_modifier(Modifier::BOLD)
  }
}

#[derive(Debug)]
pub struct App {
  puzzle: Puz,
  cursor: Cursor,
  running: bool,
}

impl App {
  fn new(puzzle: Puz) -> Self {
    let cursor = Cursor::from_grid(&puzzle.solve_state);
    Self {
      puzzle,
      cursor,
      running: true,
    }
  }

  pub fn run(mut self, mut terminal: DefaultTerminal) -> io::Result<()> {
    self.running = true;
    while self.running {
      terminal.draw(|frame| self.draw(frame))?;
      self.handle_crossterm_events()?;
    }
    Ok(())
  }

  fn draw(&self, frame: &mut Frame) {
    frame.render_widget(self, frame.area());
  }
  /// Reads the crossterm events and updates the state of [`App`].
  ///
  /// If your application needs to perform work in between handling events, you can use the
  /// [`event::poll`] function to check if there are any events available with a timeout.
  fn handle_crossterm_events(&mut self) -> io::Result<()> {
    match event::read()? {
      // it's important to check KeyEventKind::Press to avoid handling key release events
      Event::Key(key) if key.kind == KeyEventKind::Press => self.on_key_event(key),
      Event::Mouse(_) => {}
      Event::Resize(_, _) => {}
      _ => {}
    }
    Ok(())
  }

  /// Handles the key events and updates the state of [`App`].
  fn on_key_event(&mut self, key: KeyEvent) {
    match (key.modifiers, key.code) {
      (_, KeyCode::Esc | KeyCode::Char('q'))
      | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
      // Add other key handlers here.
      _ => {}
    }
  }

  /// Set running to false to quit the application.
  fn quit(&mut self) {
    self.running = false;
  }

  // Determines how a particular square should be styled.
  fn square_style(&self, pos: Pos) -> SquareStyle {
    if pos == self.cursor.pos {
      return SquareStyle::Cursor;
    }

    let (row, col) = pos;
    let (cursor_row, cursor_col) = self.cursor.pos;

    if self.cursor.direction == Across && row == cursor_row {
      let (col_start, col_end) = (min(col, cursor_col), max(col, cursor_col));
      if (col_start..col_end).any(|c| self.puzzle.solve_state.get((row, c)).is_black()) {
        return SquareStyle::Standard;
      } else {
        return SquareStyle::Word;
      }
    }

    if self.cursor.direction == Down && col == cursor_col {
      let (row_start, row_end) = (min(row, cursor_row), max(row, cursor_row));
      if (row_start..row_end).any(|r| self.puzzle.solve_state.get((r, col)).is_black()) {
        return SquareStyle::Standard;
      } else {
        return SquareStyle::Word;
      }
    }

    SquareStyle::Standard
  }

  fn current_clue(&self) -> &str {
    let pos = self.puzzle.solve_state.get_start(&self.cursor);
    let clue_number = *self.puzzle.numbered_squares.get(&pos).unwrap();
    self
      .puzzle
      .clues
      .get(&(clue_number, self.cursor.direction))
      .unwrap()
  }

  fn render_square(&self, square: Square, style: SquareStyle, square_area: Rect, buf: &mut Buffer) {
    match square {
      Square::Black => Block::new()
        .style(Style::new().bg(Color::Black))
        .render(square_area, buf),
      Square::Empty => Block::new()
        .style(Style::new().bg(Color::White))
        .render(square_area, buf),
      Square::Letter(c) => {
        Paragraph::new(c.to_string())
          .block(Block::new().style(style).padding(Padding::top(1)))
          .centered()
          .render(square_area, buf);
      }
    };
  }
}

impl Widget for &App {
  fn render(self, area: Rect, buf: &mut Buffer) {
    let [title_area, main_area] =
      Layout::vertical([Constraint::Length(2), Constraint::Percentage(100)]).areas(area);

    let title = Line::from(vec![
      "Ratatui Crossword".bold().blue(),
      ": ".bold(),
      self.puzzle.title.clone().bold(),
    ])
    .centered();
    title.render(title_area, buf);

    let [puzzle_area, clue_area] =
      Layout::horizontal([Constraint::Percentage(100), Constraint::Length(45)]).areas(main_area);

    let puzzle_area = center(
      puzzle_area,
      Constraint::Length(
        (self.puzzle.solution.width() * (1 + SQUARE_WIDTH as usize))
          .try_into()
          .unwrap(),
      ),
      Constraint::Length(
        (self.puzzle.solution.height() * (1 + SQUARE_HEIGHT as usize))
          .try_into()
          .unwrap(),
      ),
    );

    let mut square_area = Rect {
      x: puzzle_area.x,
      y: puzzle_area.y,
      width: SQUARE_WIDTH,
      height: SQUARE_HEIGHT,
    };
    for row in 0..self.puzzle.solve_state.height() {
      for col in 0..self.puzzle.solve_state.width() {
        let square = self.puzzle.solve_state.get((row, col));
        let style = self.square_style((row, col));
        self.render_square(square, style, square_area, buf);
        square_area.x += SQUARE_WIDTH + 2;
      }
      square_area.x = puzzle_area.x;
      square_area.y += SQUARE_HEIGHT + 1;
    }

    Paragraph::new(self.current_clue())
      .block(
        Block::bordered()
          .title(Line::from("Current clue").centered())
          .padding(Padding::uniform(4)),
      )
      .render(clue_area, buf);
  }
}

/// https://ratatui.rs/recipes/layout/center-a-widget/
fn center(area: Rect, horizontal: Constraint, vertical: Constraint) -> Rect {
  let [area] = Layout::horizontal([horizontal])
    .flex(Flex::Center)
    .areas(area);
  let [area] = Layout::vertical([vertical]).flex(Flex::Center).areas(area);
  area
}
