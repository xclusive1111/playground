use std::io;
use rand::Rng;
use std::cmp::Ordering;

fn main() {
    println!("Guess the number!");
    variables();
    ownership();
    play(1);
    let hello = first_word("hello world");
    println!("{}", hello);
}

fn play(turn: u32) {
    let num = rand::thread_rng().gen_range(1, 11);
    println!("What's your number?");

    let guess = read_guess();
    println!("Your guess: {}", guess);

    match guess.cmp(&num) {
        Ordering::Less => {
            println!("Too small! It's {}", num);
            play(turn + 1);
        }
        Ordering::Greater => {
            println!("Too big! It's {}", num);
            play(turn + 1);
        }
        Ordering::Equal => {
            println!("You win! It takes you {} guesses", turn);
        }
    }
}

fn plus_five(x: i32) -> i32 {
    x + 5
}

fn read_guess() -> u32 {
    let mut guess = String::new();
    io::stdin().read_line(&mut guess)
        .expect("Failed to read line");

    match guess.trim().parse() {
        Ok(n) => n,
        Err(_) => read_guess(),
    }
}

fn variables() {
    let tup: (i32, i32) = (1, 2);
    let (x, y) = tup;
    println!("Tuple: ({}, {})", x, y);

    let num = 10;
    let pf = plus_five;
    println!("{} + 5 = {}", num, pf(num));
}

fn ownership() {
    let mut name = String::from("foobar");
    say_hello(&name);
    change_name(&mut name);
    say_hello(&name);

    let n1 = &name;
    let n2 = &name;
    say_hello(n1);
    say_hello(n2);
    let mut n3 = &mut name;
    change_name(&mut n3);
}

fn say_hello(name: &String) {
    println!("Hello, {}", name);
}

fn change_name(name: &mut String) {
    *name = "FooBar".to_string();
}

fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &e) in bytes.iter().enumerate() {
        if e == b' ' {
            return &s[0..i];
        }
    }
    &s[..]
}
