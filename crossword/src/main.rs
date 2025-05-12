use crossword::ChecksumMismatch;
use crossword::{Error, Puz};
use std::env;
use std::fs;

fn parse_puz(path: &str) -> Result<(Puz, Vec<ChecksumMismatch>), Error> {
  let data: Vec<u8> = fs::read(path)?;
  Puz::parse(data)
}

/// A simple CLI for testing PUZ parsing
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
          Ok((puz, checksum_mismatches)) => {
            if checksum_mismatches.is_empty() {
              println!("Parsed '{}' successfully from {}", puz.title, p);
            } else {
              println!(
                "Parsed '{}' with checksum mismatches: {:?}",
                puz.title, checksum_mismatches
              );
            }
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
      Ok((puz, checksum_mismatches)) => {
        if checksum_mismatches.is_empty() {
          println!("Parsed '{}' successfully from {}", puz.title, path);
          dbg!(puz);
        } else {
          println!(
            "Parsed '{}' with checksum mismatches: {:?}",
            puz.title, checksum_mismatches
          );
        }
      }
      Err(e) => {
        println!("Failed with: {:?}", e);
      }
    }
  }

  Ok(())
}
