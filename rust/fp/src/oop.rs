/// Object oriented examples

pub struct AveragedCollection {
    list: Vec<i32>,
    avg: f64,
}

impl AveragedCollection {
    pub fn add(&mut self, value: i32) {
        self.list.push(value);
        self.update_avg();
    }

    pub fn avg(&self) -> f64 {
        self.avg
    }

    fn update_avg(&mut self) {
        let sum: i32 = self.list.iter().sum();
        self.avg = (sum as f64) / (self.list.len() as f64);
    }

    pub fn remove(&mut self) -> Option<i32> {
        let value = self.list.pop();
        if let Some(_) = value {
            self.update_avg();
        }
        value
    }
}

pub trait Drawable {
    fn draw(&self);
}

struct Button {
    pub width: u32,
    pub height: u32,
    pub label: String,
}

impl Drawable for Button {
    fn draw(&self) {
        println!("Draw a Button with the following dimension: w: {}, h: {}, label: {}",
                 self.width, self.height, self.label)
    }
}

struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Drawable for SelectBox {
    fn draw(&self) {
        println!("Draw a SelectBox")
    }
}

struct Screen {
    components: Vec<Box<dyn Drawable>>,
}

impl Screen {
    pub fn run(&self) {
        for comp in self.components.iter() {
            comp.draw();
        }
    }
}


struct Point {
    x: i32,
    y: i32,
}

enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
}

pub fn run() {
    let screen = Screen {
        components: vec![
            Box::new(Button {
                width: 15,
                height: 10,
                label: String::from("Choose your meal"),
            }),
            Box::new(SelectBox {
                width: 10,
                height: 5,
                options: vec![String::from("Hamburger"), String::from("Pan cake")],
            }),
        ]
    };
    screen.run();
    let mut nums = vec![Some(1), None, Some(2)];

    while let Some(Some(n)) = nums.pop() {
        println!("{}", n)
    }

    let x = 5;
    match x {
        1..=5 | 10 => println!("1 < x < 5 or x = 10"),
        _ => println!("Out of range")
    }

    let point = Point { x: 10, y: 5 };
    let Point { x, y } = point;
    println!("x = {}, y = {}", x, y);
    match point {
        Point { x: 5, .. } => println!("y = 10"),
        Point { x, y: 10 } => println!("y = 10"),
        Point { x: 10, y: 5 } => println!("x = 10, y = 5"),
        _ => ()
    }

    let msg = Message::Write(String::from("Oh yeah!"));
    match msg {
        Message::Quit => println!("Quiting"),
        Message::Move { x, y: i32 } => println!("Moving x = {}, y = {}", x, y),
        Message::Move { x: x_value @ 1..=8, y: i32 } => println!("Moving 1 < x < 8, y = {}", y),
        Message::Write(s) if s.eq(&String::from("Opps!")) => println!("it's a string"),
        Message::Write(s) => println!("{}", s),
    }
}
