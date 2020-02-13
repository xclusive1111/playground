mod point;

fn main() {
    let mut p1 = point::new_point(5, 10);
    println!("p.x = {}", p1.x());
    p1.move_left(10);
    println!("p.x = {}", p1.x());
    p1.move_right(10);
    println!("px = {}, p.y = {}", p1.x(), p1.y());
}
