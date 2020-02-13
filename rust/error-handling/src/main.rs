use std::fs::File;
use std::io;
use std::io::Read;

fn main() {
    println!("Hello, world!");
    let path = String::from("/tmp/abcd");
    let rs1 = read_user(&path);
    let rs2 = read_user_2(&path);
}


fn read_user(path: &String) -> Result<String, io::Error> {
    let f = File::open(path);
    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();
    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e)
    }
}

fn read_user_2(path: &String) -> Result<String, io::Error> {
    let mut f = File::open(path)?;
    let mut s = String::new();
    println!("Reading file...");
    f.read_to_string(&mut s)?;
    println!("Return file's content");
    Ok(s)
}