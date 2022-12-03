use std::env::args;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

fn split_on<T, TS, F>(input: TS, predicate: F) -> Vec<Vec<T>>
where
    F: Fn(T) -> bool,
    TS: Iterator<Item = T>,
    T: Copy,
{
    input.fold(vec![], |mut acc, x: T| match (predicate(x), acc.last_mut()) {
        (true, _) | (_, None) => {
            acc.push(vec![x]);
            acc
        }
        (false, Some(v)) => {
            v.push(x);
            acc
        }
    })
}

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = args().nth(1).map(PathBuf::from).expect("Expected path argument");
    let grouped_input = split_on(fs::read_to_string(input_path)?.lines(), &str::is_empty);
    println!("Hello, world!");
    Ok(())
}
