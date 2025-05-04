use std::env;
use std::fs;

use puzparser::{Error, Puz};

fn parse_puz(path: &str) -> Result<Puz, Error> {
  let data: Vec<u8> = fs::read(path)?;
  Puz::parse(data)
}

fn main() -> Result<(), Error> {
  let args: Vec<String> = env::args().collect();

  let path = &args[1];
  if fs::metadata(path)?.is_dir() {
    let mut success = 0;
    let mut failure = 0;

    for entry in fs::read_dir(path)? {
      let puz_path = entry.unwrap().path();
      if let Some(p) = puz_path.to_str() {
        match parse_puz(p) {
          Ok(puz) => {
            println!("Parsed {} successfully from {}", puz.title, p);
            success += 1;
          }
          Err(e) => {
            println!("Failed with {:?} from {}", e, p);
            failure += 1;
          }
        }
      }
    }
    dbg!(success, failure);
  } else {
    match parse_puz(path) {
      Ok(puz) => {
        println!("Parsed {} successfully", puz.title);
      }
      Err(e) => {
        println!("Failed with: {:?}", e);
      }
    }
  }

  Ok(())
}
