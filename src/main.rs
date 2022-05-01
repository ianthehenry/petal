use dim::{parser, tokenizer};
use std::{fs, path::PathBuf};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(about = "dim array language")]
enum Command {
    Run {
        #[structopt(parse(from_os_str))]
        filenames: Vec<PathBuf>,
        #[structopt(short)]
        inline: bool,
    },
    Eval {
        expression: String,
    },
}

fn main() {
    match Command::from_args() {
        Command::Run { filenames, inline } => {
            println!("{:?} {}", filenames, inline);
            for filename in filenames {
                let contents = fs::read_to_string(filename).expect("unable to read file");
                let (remaining, tokens) = tokenizer::tokens(&contents).unwrap();
                if !remaining.is_empty() {
                    panic!("not a total parse!");
                }
                match parser::parse_tokens(tokens) {
                    Ok(annotated_term) => println!("{:?}", annotated_term),
                    Err(parser::ParseError::DidNotFullyReduce(terms)) => {
                        panic!("incomplete parse: {:?}", terms)
                    }
                    Err(error) => panic!("error: {:?}", error),
                }
            }
        }
        Command::Eval { expression } => println!("{}", expression),
    }
}
