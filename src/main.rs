use dim_syntax::{new_parser, pos_parser, tokenizer};
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
                let tokens = crate::tokenizer::tokenize(input);
                let terms = crate::new_parser::parse_expression(tokens).unwrap();
                let terms = crate::semicolons::resolve_expression(terms);
                let terms = crate::op_splitter::split_expression(terms);

                pos_parser::just_parse(terms).unwrap();
            }
        }
        Command::Eval { expression } => println!("{}", expression),
    }
}
