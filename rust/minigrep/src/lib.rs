use std::{fs, env};
use std::error::Error;

pub struct Config {
    query: String,
    filename: String,
    pub case_sensitive: bool,
}

impl Config {
    // The 'static is a special lifetime that can life for the
    // entire duration of the program.
    pub fn new(args: &[String]) -> Result<Self, &'static str> {
        if args.len() < 3 {
            return Err("not enough arguments");
        }

        let query = args[1].clone();
        let filename = args[2].clone();

        // If the env is not set, do a case sensitive search.
        let case_sensitive = env::var("CASE_INSENSITIVE").is_err();

        Ok(Config { query, filename, case_sensitive })
    }
}

pub fn run(conf: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(conf.filename)?;
    let search_fn = if conf.case_sensitive {
        search_case_sensitive
    } else {
        search_case_insensitive
    };

    for line in search_fn(&conf.query, &contents) {
        println!("{}", line);
    }
    Ok(())
}

fn search_case_sensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    contents
        .lines()
        .filter(|line| line.contains(query))
        .collect()
}

fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let query = query.to_lowercase();
    let mut results = Vec::new();
    for line in contents.lines() {
        if line.to_lowercase().contains(&query) {
            results.push(line);
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn case_sensitive() {
        let query = "jumps";
        let contents = "\
The quick
brown fox jumps over
the lazy dog.";

        assert_eq!(
            vec!["brown fox jumps over"],
            search_case_sensitive(query, contents)
        );
    }

    #[test]
    fn case_insensitive() {
        let query = "tHe";
        let contents = "\
The quick
brown fox jumps over
the lazy dog.";
        assert_eq!(
            vec!["The quick", "the lazy dog."],
            search_case_insensitive(query, contents),
        );
    }
}
