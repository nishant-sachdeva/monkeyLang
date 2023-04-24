use std::{
    env::Args,
    path::PathBuf,
};

pub struct Config {
    pub file_name: Box<PathBuf>,
}

impl Config {
    pub fn build(mut args: Args) -> Result<Config, &'static str> {
        args.next();

        let file_name = match args.next() {
            Some(arg) => arg,
            None => return Err("Didn't get a file name"),
        };        

        return Ok (Config {
            file_name: Box::new(PathBuf::from(file_name)),
        });
    }
    
}
