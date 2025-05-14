# Crossword

A library for crossword puzzles!

## Playing crosswords

This crate comes with no UI so if you want to actually play a crossword, try `crosstui` which is backed by this crate.

## Writing your own crossword app

If you want to use this crate to build your own crossword app, `Puzzle` is the main type you will use. Use `Puzzle::parse` to get a puzzle from a `.puz` file. See `crosstui` for an example.
