pub use monkey_lang::{
    config::Config,
    interpreter::evaluate,
    start_interpreter,
    start_repl,
    start_compiler_repl
};

fn main() {
    match Config::build(std::env::args()) {
        Some(config) => {
            println!("Running file: {:?}", config.file_name);
            start_interpreter(&config)
        },
        None => {
            println!("Running REPL");
            start_compiler_repl()
        }
    };
}
