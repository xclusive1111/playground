mod smart_pointers;
mod concurrency;
mod oop;

use smart_pointers::b;
use smart_pointers::c;
use concurrency::d;
use concurrency::e;
use concurrency::f;
use std::iter::Iterator;

fn main() {
    b::run();
    c::run();
    d::run();
    e::run();
    f::run();
    oop::run();
    let mut counter = Counter { value: 0 };
    while let Some(val) = counter.next() {
        println!("{}", val);
    }

    let mut counter = Counter { value: 0 };
    while let Some(val) = counter.next2() {
        println!("{}", val);
    }

    let nums = vec![1, 2, 3, 4];
    let nums: Vec<i32> = nums.iter()
        .filter(|&n| is_even(*n))
        .map(|n| double(*n))
        .collect();
    for n in nums {
        println!("{}", n);
    }

    let things = vec![Thing{ value: 0}, Thing{value: 1}, Thing{value: 2}, Thing{value: 3}];
    let things: Vec<Thing> = things.iter()
        .filter(|t| is_even(t.value))
        .map(|t| Thing{value: double(t.value)})
        .collect();

    for t in things {
        println!("{:?}", t);
    }

    let things = vec![Thing{ value: 0}, Thing{value: 1}, Thing{value: 2}, Thing{value: 3}];
    let things: Vec<&Thing> = things.iter()
        .filter(|t| is_even_thing(t))
        .collect();

    for t in things {
        println!("{:?}", t);
    }

    let sfn = get_fn();
    let x = sfn(1);
    println!("{}", x);
}

fn get_fn() -> Box<dyn Fn(i32) -> String> {
    Box::new(|x| format!("{} Oh year", x))
}

#[derive(Debug)]
struct Thing {
    value: i32,
}

fn is_even_thing(t: &Thing) -> bool {
    is_even(t.value)
}

fn is_even(n: i32) -> bool {
    n % 2 == 0
}

fn double(n: i32) -> i32 {
    n * 2
}

pub trait Iter {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
}

pub trait Iterator2<T> {
    fn next2(&mut self) -> Option<T>;
}

struct Counter {
    value: u32,
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.value < 10 {
            self.value += 1;
            return Some(self.value);
        }
        None
    }
}

impl Iterator2<String> for Counter {
    fn next2(&mut self) -> Option<String> {
        if self.value < 10 {
            self.value += 1;
            return Some(String::from("Keep going"));
        }
        None
    }
}

