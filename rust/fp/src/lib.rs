use std::thread;
use std::time::Duration;
use std::collections::HashMap;

pub struct Cacher<T>
    where T: Fn(u32) -> u32 {
    calculation: T,
    value: HashMap<u32, u32>,
}

impl<T> Cacher<T>
    where T: Fn(u32) -> u32 {
    fn new(calculation: T) -> Self {
        Cacher { calculation, value: HashMap::new() }
    }

    fn value(&mut self, arg: u32) -> u32 {
        match self.value.get(&arg) {
            Some(v) => *v,
            None => {
                let v = (self.calculation)(arg);
                self.value.insert(arg, v);
                v
            }
        }
    }
}

struct Counter {
    count: u32,
}

impl Counter {
    fn new() -> Self {
        Counter { count: 0 }
    }
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;
        if self.count < 6 {
            Some(self.count)
        } else {
            None
        }
    }
}

pub fn generate_workout(intensity: u32, random_number: u32) {
    let mut expensive_result = Cacher::new(|num: u32| {
        println!("Calculating...");
        thread::sleep(Duration::from_secs(2));
        num
    });

    if intensity < 25 {
        println!(
            "Today, do {} pushups!",
            expensive_result.value(intensity)
        );
        println!(
            "Next, do {} situps!",
            expensive_result.value(intensity)
        );
    } else {
        if random_number == 3 {
            println!("Take a break today! Remember to stay hydrated!");
        } else {
            println!(
                "Today, run for {} minutes!",
                expensive_result.value(intensity)
            );
        }
    }
}

#[derive(PartialEq, Debug)]
struct Shoe {
    size: u32,
    style: String,
}

fn find_shoe_by_size(shoes: Vec<Shoe>, shoe_size: u32) -> Vec<Shoe> {
    shoes.into_iter()
        .filter(|shoe| shoe.size == shoe_size)
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn cache_with_different_values() {
        let mut c = Cacher::new(|a| a);
        let _v1 = c.value(1);
        let v2 = c.value(2);
        assert_eq!(v2, 2);
    }

    #[test]
    fn iterator_test() {
        let v1 = vec![1, 2, 3, -4];
        let v1_iter = v1.iter();

        // The for loop takes ownership of the v1_iter behind the scene.
        for x in v1_iter {
            println!("{}", x);
        }

        // This won't compile because v1_iter is already moved.
        // let s: u32 = v1_iter.sum();

        let sum: i32 = v1.iter().sum();
        println!("{}", sum);

        let x: Vec<i32> = v1.iter()
            .map(|x| x + 1)
            .collect();
    }

    #[test]
    fn filters_shoes_by_size() {
        let shoes = vec![
            Shoe { size: 10, style: String::from("sneaker") },
            Shoe { size: 13, style: String::from("sandal") },
            Shoe { size: 10, style: String::from("boot") },
        ];

        let found = find_shoe_by_size(shoes, 10);
        assert_eq!(
            found,
            vec![
                Shoe { size: 10, style: String::from("sneaker") },
                Shoe { size: 10, style: String::from("boot") },
            ]
        );
    }

    #[test]
    fn counter() {
        let mut c = Counter::new();
        assert_eq!(Some(1), c.next());
        assert_eq!(Some(2), c.next());
        assert_eq!(Some(3), c.next());
        assert_eq!(Some(4), c.next());
        assert_eq!(Some(5), c.next());
        assert_eq!(None, c.next());

        let mut c = Counter::new().skip(3);
        assert_eq!(Some(4), c.next());

        let c1 = Counter::new();
        let c2 = Counter::new();
        let x = c1
            .zip(c2.skip(1))
            .map(|(a, b)| a * b)
            .filter(|x| x % 3 == 0);

        let x: u32 = x.sum();
    }
}
