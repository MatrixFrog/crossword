use std::{env, io};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use puzparser::{Puz, Square};
use ratatui::{
  DefaultTerminal, Frame,
  buffer::Buffer,
  layout::{Constraint, Flex, Layout, Rect},
  style::{Color, Style, Stylize},
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

  let app = App::new(&args[1])?;

  let terminal = ratatui::init();
  let result = app.run(terminal);
  ratatui::restore();
  result
}

#[derive(Debug)]
pub struct App {
  puzzle: Puz,
  running: bool,
}

impl App {
  pub fn new(path: &str) -> Result<Self, io::Error> {
    let data: Vec<u8> = std::fs::read(path)?;
    let (puzzle, checksum_mismatches) = Puz::parse(data).unwrap_or_else(|e| {
      println!("Failed to parse .puz file: {:?}", e);
      std::process::exit(2);
    });

    if checksum_mismatches.is_empty() {
      Ok(Self {
        puzzle,
        running: true,
      })
    } else {
      todo!("Handle checksum errors")
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
}

impl Widget for &App {
  fn render(self, area: Rect, buf: &mut Buffer) {
    let vertical_layout = Layout::vertical([Constraint::Length(2), Constraint::Percentage(100)]);
    let [title_area, main_area] = vertical_layout.areas(area);

    let title = Line::from(vec![
      "Ratatui Crossword".bold().blue(),
      ": ".bold(),
      self.puzzle.title.clone().bold(),
    ])
    .centered();
    title.render(title_area, buf);

    let puzzle_area = center(
      main_area,
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
        match square {
          Square::Black => Block::new()
            .style(Style::new().bg(Color::Black))
            .render(square_area, buf),
          Square::Empty => Block::new()
            .style(Style::new().bg(Color::White))
            .render(square_area, buf),
          Square::Letter(c) => {
            Paragraph::new(c.to_string())
              .block(
                Block::new()
                  .style(Style::new().bg(Color::White).fg(Color::Black))
                  .padding(Padding::top(1)),
              )
              .centered()
              .render(square_area, buf);
          }
        };
        square_area.x += SQUARE_WIDTH + 2;
      }
      square_area.x = puzzle_area.x;
      square_area.y += SQUARE_HEIGHT + 1;
    }
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
