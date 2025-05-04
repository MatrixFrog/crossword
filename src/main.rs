use std::env;
use std::fs;

use puzparser::{Error, Puz};

fn main() -> Result<(), Error> {
  let args: Vec<String> = env::args().collect();

  let data: Vec<u8> = fs::read(&args[1])?;
  let puz = Puz::parse(data)?;
  // println!("{:?}", puz);

  Ok(())
}
