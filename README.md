# Crossword and Crosstui

This is an experimental project for solving crosswords in the terminal. I built it mainly because I wanted to build something fun with [Ratatui], a library for TUIs (user interfaces that run in the terminal).

It's still a work in progress but if you like crosswords and running things in the terminal, please give it a try!

## To play a crossword

First, find a `.puz` file. Many crossword sites allow you to download puzzles in this format, such as <https://crosshare.org/> or <https://crosswordfiend.com/download/>. Or, use one of the examples in this repo, in `crossword/puzzles/`.

Then, run the `crosstui` binary, passing the path to the `.puz` file as an argument:

```sh
cargo run --release --bin=crosstui -- /path/to/your/crossword.puz
```

## Code organization

The code is separated into two crates.

The `crossword` crate understands `.puz` files and how crossword puzzles work, but has no UI. Its primary type is the `Puzzle` struct, and it should be possible to build a separate crossword app using a different UI framework, by depending on `crossword` only.

The `crosstui` crate is the interactive UI for displaying and solving puzzles, which uses the `crossword` crate to model the puzzle, and [Ratatui] for the UI.

[Ratatui]: https://ratatui.rs/
