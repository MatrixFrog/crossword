use std::io;
use std::path::{Path, PathBuf};

use clap::Parser;
use clap::builder::Styles;
use clap::builder::styling::AnsiColor;
use crossterm::event::{self, KeyCode, KeyEvent};
use crossword::{Direction, Puzzle, Square, SquareStyle};
use ratatui::DefaultTerminal;
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Style, Stylize};
use ratatui::text::Line;
use ratatui::widgets::{Block, List, ListState, Padding, Paragraph, StatefulWidget, Widget, Wrap};
use ratatui_macros::{line, text};

const SQUARE_WIDTH: u16 = 7;
const SQUARE_HEIGHT: u16 = 3;

const HELP_STYLES: Styles = Styles::styled()
    .header(AnsiColor::Blue.on_default().bold())
    .usage(AnsiColor::Blue.on_default().bold())
    .literal(AnsiColor::White.on_default())
    .placeholder(AnsiColor::Green.on_default());

#[derive(Parser, Debug)]
#[command(version, about, author, styles = HELP_STYLES)]
struct Cli {
    /// The path to the .puz file
    path: PathBuf,
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let puzzle = load_puzzle(&args.path);
    let app = App::new(puzzle);
    ratatui::run(|terminal| app.run(terminal))
}

pub fn load_puzzle(path: &Path) -> Puzzle {
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

    pub fn run(mut self, terminal: &mut DefaultTerminal) -> io::Result<()> {
        self.running = true;
        while self.running {
            terminal.draw(|frame| frame.render_widget(&self, frame.area()))?;
            self.handle_crossterm_events()?;
        }
        Ok(())
    }

    /// Reads the crossterm events and updates the state of [`App`].
    fn handle_crossterm_events(&mut self) -> io::Result<()> {
        if let Some(key) = event::read()?.as_key_press_event() {
            self.on_key_event(key);
        }
        Ok(())
    }

    /// Handles the key events and updates the state of [`App`].
    fn on_key_event(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Esc => self.quit(),
            KeyCode::Up => self.puzzle.cursor_up(),
            KeyCode::Down => self.puzzle.cursor_down(),
            KeyCode::Left => self.puzzle.cursor_left(),
            KeyCode::Right => self.puzzle.cursor_right(),
            KeyCode::Backspace => {
                self.puzzle.erase_letter();
                self.puzzle.backup_cursor();
            }
            KeyCode::Delete => self.puzzle.erase_letter(),
            KeyCode::Tab => self.puzzle.advance_cursor_to_next_word(),
            KeyCode::Char(' ') => self.puzzle.swap_cursor_direction(),
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
}

impl Widget for &App {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let vertical = Layout::vertical([Constraint::Length(2), Constraint::Fill(1)]);
        let [title_area, main_area] = area.layout(&vertical);

        let title = text![
            "",
            line!["Crosstui".light_blue(), ": ", self.puzzle.title()]
                .bold()
                .centered(),
        ];
        title.render(title_area, buf);

        let horizontal = Layout::horizontal([Constraint::Length(45), Constraint::Percentage(100)]);
        let [left_area, puzzle_area] = main_area.layout(&horizontal);

        PuzzleGrid::new(&self.puzzle).render(puzzle_area, buf);

        let layout = Layout::vertical([
            Constraint::Length(5),
            Constraint::Length(4),
            Constraint::Fill(1),
            Constraint::Fill(1),
            Constraint::Length(8),
        ]);
        let [
            instructions_area,
            current_clue_area,
            across_clue_area,
            down_clue_area,
            metadata_area,
        ] = left_area.layout(&layout);

        let instructions = line![
            "Instructions: ".bold(),
            "Use the arrow keys, space, and tab, to navigate the puzzle. Press escape to exit."
                .gray(),
        ];
        Paragraph::new(instructions)
            .wrap(Wrap::default())
            .render(instructions_area, buf);

        if self.puzzle.is_solved() {
            Paragraph::new("You solved it!")
                .block(
                    Block::bordered()
                        .title(Line::from(" Congratulations! ").centered())
                        .padding(Padding::uniform(2)),
                )
                .render(current_clue_area, buf)
        } else {
            let (num, direction) = self.puzzle.current_clue_identifier();
            Paragraph::new(line![
                format!("{}{}", num, direction.to_char()).light_red(),
                ". ",
                self.puzzle.current_clue()
            ])
            .wrap(Wrap::default())
            .render(current_clue_area, buf);
        }

        ClueList::new(&self.puzzle, Direction::Across).render(across_clue_area, buf);
        ClueList::new(&self.puzzle, Direction::Down).render(down_clue_area, buf);

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

// A widget that renders the puzzle grid.
struct PuzzleGrid<'a> {
    puzzle: &'a Puzzle,
}

impl<'a> PuzzleGrid<'a> {
    /// Creates a new `PuzzleGrid` widget.
    pub fn new(puzzle: &'a Puzzle) -> Self {
        Self { puzzle }
    }
}

impl Widget for PuzzleGrid<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let grid = self.puzzle.grid();
        let grid_area = area.centered(
            Constraint::Length(grid.width() as u16 * (2 + SQUARE_WIDTH)),
            Constraint::Length(grid.height() as u16 * (1 + SQUARE_HEIGHT)),
        );

        for row in 0..grid.height() {
            for col in 0..grid.width() {
                let square = grid.get((row, col));
                let style = to_ratatui_style(self.puzzle.square_style((row, col)));
                let square_area = Rect {
                    x: grid_area.x + col as u16 * (SQUARE_WIDTH + 2),
                    y: grid_area.y + row as u16 * (SQUARE_HEIGHT + 1),
                    width: SQUARE_WIDTH,
                    height: SQUARE_HEIGHT,
                };
                render_square(square, style, square_area, buf);
            }
        }
    }
}

/// A widget that renders a list of clues
struct ClueList<'a> {
    puzzle: &'a Puzzle,
    direction: Direction,
}

impl<'a> ClueList<'a> {
    pub fn new(puzzle: &'a Puzzle, direction: Direction) -> Self {
        Self { puzzle, direction }
    }
}

impl Widget for ClueList<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let current_clue_identifier = self.puzzle.current_clue_identifier();
        let cross_clue_identifier = self.puzzle.cross_clue_identifier();

        let mut list_state = ListState::default();
        let lines = self
            .puzzle
            .clues(self.direction)
            .into_iter()
            .enumerate()
            .map(|(index, (num, clue))| {
                if current_clue_identifier == (num, self.direction)
                    || cross_clue_identifier.is_some_and(|cross_clue_identifier| {
                        cross_clue_identifier == (num, self.direction)
                    })
                {
                    list_state.select(Some(index));
                };
                line![num.to_string().light_red(), ". ", clue]
            })
            .collect::<Vec<_>>();

        let highlight_style = if self.direction == self.puzzle.cursor_direction() {
            Style::default().black().bold().on_light_yellow()
        } else {
            Style::default().blue().bold().on_gray()
        };

        let clue_list = List::new(lines).highlight_style(highlight_style).block(
            Block::bordered()
                .title(line![" ", self.direction.to_string(), " clues "].centered())
                .padding(Padding {
                    left: 2,
                    right: 2,
                    top: 1,
                    bottom: 1,
                }),
        );

        StatefulWidget::render(clue_list, area, buf, &mut list_state);
    }
}

fn to_ratatui_style(value: SquareStyle) -> Style {
    let bg = match value {
        SquareStyle::Standard => Color::White,
        SquareStyle::Cursor => Color::LightRed,
        SquareStyle::Word => Color::LightYellow,
    };
    Style::new().bg(bg).black().bold()
}

fn render_square(square: Square, style: Style, area: Rect, buf: &mut Buffer) {
    match square {
        Square::Black => Block::new().on_black().render(area, buf),
        Square::Empty => Block::new().style(style).render(area, buf),
        Square::Letter(c) => {
            Paragraph::new(c.to_string())
                .block(Block::new().style(style).padding(Padding::top(1)))
                .centered()
                .render(area, buf);
        }
    };
}
