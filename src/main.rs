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
                dim_syntax::parse(&contents).unwrap();
            }
        }
        Command::Eval { expression } => println!("{}", expression),
    }
}
