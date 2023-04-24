pub mod config;
pub mod interpreter;

pub fn run_interpreter(_config: &config::Config) -> Result<bool, String>{
    Ok(true)
}
