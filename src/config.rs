use std::{
    env::Args,
    path::PathBuf,
};

pub struct Config {
    pub file_name: Box<PathBuf>,
}

impl Config {
    pub fn build(mut args: Args) -> Option<Config> {
        args.next();

        let file_name = match args.next() {
            Some(arg) => arg,
            None => return None,
        };        

        return Some (Config {
            file_name: Box::new(PathBuf::from(file_name)),
        });
    }
    
}
