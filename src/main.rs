pub use monkey_lang::{
    config::Config,
    interpreter::evaluate,
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
        evaluate::object_system::Object::Integer(i) => println!("{:?}", i),
        evaluate::object_system::Object::Boolean(b) => println!("{:?}", b),
        evaluate::object_system::Object::Null => println!("null"),
        evaluate::object_system::Object::EvalError(e) => println!("{:?}", e),
        evaluate::object_system::Object::ReturnValue(r) => println!("{:?}", r),
        evaluate::object_system::Object::FunctionObject(f) => println!("{:?}", f),
        evaluate::object_system::Object::StringObject(s) => println!("{:?}", s),
        evaluate::object_system::Object::ArrayObject(a) => println!("{:?}", a),
        evaluate::object_system::Object::HashObject(h) => println!("{:?}", h),
        evaluate::object_system::Object::BuiltinFunctionObject(b) => println!("{:?}", b),
    }
}
