use std::{env, io};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use crossword::{Puzzle, Square, SquareStyle};
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Flex, Layout, Rect};
use ratatui::style::{Color, Modifier, Style, Stylize};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Block, Padding, Paragraph, Widget, Wrap};
use ratatui::{DefaultTerminal, Frame};

const SQUARE_WIDTH: u16 = 7;
const SQUARE_HEIGHT: u16 = 3;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Pass the filename of a .puz file to play");
        std::process::exit(1);
    }

    let puzzle = parse_puzzle(&args[1]);
    let app = App::new(puzzle);

    let terminal = ratatui::init();
    let result = app.run(terminal);
    ratatui::restore();
    result
}

pub fn parse_puzzle(path: &str) -> Puzzle {
    let data: Vec<u8> = std::fs::read(path).unwrap_or_else(|err| {
        println!("{:?}", err);
        std::process::exit(1);
    });
    let (puzzle, checksum_mismatches) = Puzzle::parse(data).unwrap_or_else(|e| {
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

fn to_ratatui_style(value: SquareStyle) -> Style {
    let base_style = match value {
        SquareStyle::Standard => Style::new().bg(Color::White),
        SquareStyle::Cursor => Style::new().bg(Color::LightRed),
        SquareStyle::Word => Style::new().bg(Color::LightYellow),
    };
    base_style.fg(Color::Black).add_modifier(Modifier::BOLD)
}

#[derive(Debug)]
pub struct App {
    puzzle: Puzzle,
    running: bool,
}

impl App {
    fn new(puzzle: Puzzle) -> Self {
        Self {
            puzzle,
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
        match key.code {
            KeyCode::Esc => self.quit(),
            KeyCode::Up => {
                self.puzzle.cursor_up();
            }
            KeyCode::Down => {
                self.puzzle.cursor_down();
            }
            KeyCode::Left => {
                self.puzzle.cursor_left();
            }
            KeyCode::Right => {
                self.puzzle.cursor_right();
            }
            KeyCode::Backspace => {
                self.puzzle.erase_letter();
                self.puzzle.backup_cursor();
            }
            KeyCode::Delete => {
                self.puzzle.erase_letter();
            }
            KeyCode::Tab => {
                self.puzzle.advance_cursor_to_next_word();
            }
            KeyCode::Char(' ') => {
                self.puzzle.swap_cursor_direction();
            }
            KeyCode::Char(letter) => {
                if letter.is_ascii_alphabetic() {
                    self.puzzle.add_letter(letter);
                    self.puzzle.move_cursor_to_next_empty_in_current_word();
                }
            }
            _ => {}
        }
    }

    /// Set running to false to quit the application.
    fn quit(&mut self) {
        self.running = false;
    }

    fn render_square(&self, square: Square, style: Style, square_area: Rect, buf: &mut Buffer) {
        match square {
            Square::Black => Block::new()
                .style(Style::new().bg(Color::Black))
                .render(square_area, buf),
            Square::Empty => Block::new().style(style).render(square_area, buf),
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

        let title = Text::from(vec![
            Line::from(""),
            Line::from(vec![
                "Crosstui".light_blue(),
                ": ".into(),
                self.puzzle.title().into(),
            ])
            .bold()
            .centered(),
        ]);
        title.render(title_area, buf);

        let [puzzle_area, right_area] =
            Layout::horizontal([Constraint::Percentage(100), Constraint::Length(45)])
                .areas(main_area);

        let puzzle_area = center(
            puzzle_area,
            Constraint::Length(
                (self.puzzle.grid().width() * (2 + SQUARE_WIDTH as usize))
                    .try_into()
                    .unwrap(),
            ),
            Constraint::Length(
                (self.puzzle.grid().height() * (1 + SQUARE_HEIGHT as usize))
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
        for row in 0..self.puzzle.grid().height() {
            for col in 0..self.puzzle.grid().width() {
                let square = self.puzzle.grid().get((row, col));
                let style = to_ratatui_style(self.puzzle.square_style((row, col)));
                self.render_square(square, style, square_area, buf);
                square_area.x += SQUARE_WIDTH + 2;
            }
            square_area.x = puzzle_area.x;
            square_area.y += SQUARE_HEIGHT + 1;
        }

        let [instructions_area, clue_area, metadata_area] = Layout::vertical([
            Constraint::Length(10),
            Constraint::Percentage(100),
            Constraint::Length(15),
        ])
        .areas(right_area);

        Paragraph::new(Line::from(vec![
            "Instructions: ".bold(),
            "Use the arrow keys, space, and tab, to navigate the puzzle. Press escape to exit."
                .gray(),
        ]))
        .wrap(Wrap::default())
        .render(instructions_area, buf);

        if self.puzzle.is_solved() {
            Paragraph::new("You solved it!")
                .block(
                    Block::bordered()
                        .title(Line::from(" Congratulations! ").centered())
                        .padding(Padding::uniform(4)),
                )
                .render(clue_area, buf)
        } else {
            Paragraph::new(self.puzzle.current_clue())
                .wrap(Wrap::default())
                .block(
                    Block::bordered()
                        .title(Line::from(" Current clue ").centered())
                        .padding(Padding::uniform(4)),
                )
                .render(clue_area, buf);
        }

        let mut metadata: Vec<Line> = vec!["".into()];
        let author = self.puzzle.author();
        if !author.is_empty() {
            metadata.push("".into());
            metadata.push(Line::from(vec!["Author: ".bold(), author.into()]))
        }
        let notes = self.puzzle.notes();
        if !notes.is_empty() {
            metadata.push("".into());
            metadata.push(notes.into());
        }
        let copyright = self.puzzle.copyright();
        if !copyright.is_empty() {
            metadata.push("".into());
            metadata.push(copyright.into());
        }

        Paragraph::new(metadata)
            .wrap(Wrap::default())
            .render(metadata_area, buf);
    }
}

/// <https://ratatui.rs/recipes/layout/center-a-widget/>
fn center(area: Rect, horizontal: Constraint, vertical: Constraint) -> Rect {
    let [area] = Layout::horizontal([horizontal])
        .flex(Flex::Center)
        .areas(area);
    let [area] = Layout::vertical([vertical]).flex(Flex::Center).areas(area);
    area
}
