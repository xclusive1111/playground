// Enum, struct & pattern matching

fn main() {
    let rect = Rectangle{ width: 15, height: 20 };
    let rect2 = Rectangle{ width: 10, height: 15 };
    println!("Area = {} square pixels", area(&rect));
    println!("Area = {} square pixels", rect.area());
    println!("w = {}, y = {}", rect.width, rect.height);
    println!("Rect is {:?}", rect);
    println!("Rect2 is {:?}", rect2);
    println!("Rect can hold rect2: {}", rect.can_hold(&rect2));

    let rect3 = Rectangle::square(24);
    println!("Rect3 is a square: {}", is_square(&rect3));
    println!("{:?}", rect3);

    println!("One dime equals {} cents", value_in_cents(Coin::Dime));
    println!("5 quaters equals {} cents", value_in_cents(Coin::Quarter(5)));
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
       self.width > other.width && self.height > other.height
    }

    fn square(size: u32) -> Rectangle {
       Rectangle{ width: size, height: size }
    }
}

fn area(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(u32),
}

fn is_square(rect: &Rectangle) -> bool {
    match rect {
        Rectangle{ width: w, height: h } => w == h,
        _ => false
    }
}

fn value_in_cents(coin: Coin) -> u32 {
    if let Coin::Quarter(amount) = coin {
        println!("Matching a quater with amount of {}", amount);
    } else {
        println!("Matching a coin");
    }

    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(amount) => amount * 25,
    }
}
