use dim::{new_parser, new_tokenizer, parser, tokenizer};
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
                let tokens = tokenizer::tokenize(&contents);
                let new_tokens = new_tokenizer::tokenize(&contents);
                new_parser::parse_tokens(new_tokens).unwrap();
                parser::just_parse(tokens).unwrap();
            }
        }
        Command::Eval { expression } => println!("{}", expression),
    }
}
