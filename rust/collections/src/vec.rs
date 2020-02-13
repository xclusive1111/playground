pub fn new_vector() -> Vec<i32> {
    let v: Vec<i32> = Vec::new();
    let s = String::from("");
    println!("a string {}", s);
    println!("{:?}", v);
    println!("{:?}", v);

    let v = vec![1, 2, 3];
    println!("{:?}", v);
    v
}
