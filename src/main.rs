pub use monkey_lang::{
    config::Config,
    run_interpreter
};

fn main() {
    let config = match Config::build(std::env::args()) {
        Ok(config) => config,
        Err(err) => {
            eprintln!("Error {}", err);
            std::process::exit(1);
        }
    };
    match run_interpreter(&config) {
        Ok(result) => {
            assert_eq!(result, true);
            std::process::exit(0);
        }
        Err(err) => {
            eprintln!("Error {}", err);
            std::process::exit(1);
        }
    }
}
