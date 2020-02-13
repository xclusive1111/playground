pub mod vec;
pub mod fun;
pub mod str;
pub mod hashmap;

fn main() {
    let mut v = vec::new_vector();
    v.push(1);
    v.push(2);
    println!("{:?}", v);
    let first = &v[0];
    println!("The first element is {}", first);
    v.push(3);
    let second = &v[1];
    println!("The second element is {}", second);

    // Below code will not compile.
    // println!("The first element is {}", first);

    // Double each element
    for e in &mut v {
        *e = *e * 2;
        // *e *= 2;
    }

    let last = v.pop();
    println!("The last element is {:?}", last);

    for e in &v {
        println!("{}", e);
    }

    let cells = new_cells();

    for cell in &cells {
        println!("{:?}", cell);
    }

    // Crash
    // let first_cell = &cells[4];

    let s = str::tic_tac_toe();
    println!("{}", s);

    let m = hashmap::new_hashmap();
    println!("{:?}", m);

    let key = String::from("foo");
    let value = m.get(&key);
    println!("{} => {:?}", key, value);

    for (k, v) in &m {
        println!("{} => {}", k, v);
    }
}

#[derive(Debug)]
enum Cell {
    Int(i32),
    Float(f64),
    Text(String),
}

fn new_cells() -> Vec<Cell> {
    vec![
        Cell::Int(10),
        Cell::Float(12.3),
        Cell::Text(String::from("foobar")),
    ]
}
