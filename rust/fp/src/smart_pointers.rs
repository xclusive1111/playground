pub mod b {
    use std::ops::Deref;

    pub enum List {
        Cons(i32, Box<List>),
        Nil,
    }

    pub struct MyBox<T>(T);

    impl<T> MyBox<T> {
        pub fn new(x: T) -> Self {
            MyBox(x)
        }
    }

    impl<T> Deref for MyBox<T> {
        type Target = T;

        fn deref(&self) -> &T {
            &self.0
        }
    }

    impl<T> Drop for MyBox<T> {
        // This fn will run when an instance of MyBox goes out of scope.
        fn drop(&mut self) {
            println!("Dropping MyBox")
        }
    }

    pub fn run() {
        let intensity = 10;
        let random_number = 7;
        fp::generate_workout(intensity, random_number);
        let list = List::Cons(1,
                        Box::new(List::Cons(2,
                                      Box::new(List::Cons(3,
                                                    Box::new(List::Nil))))));
        let x = 5;
        let y = Box::new(x);
        assert_eq!(5, x);
        assert_eq!(5, *y);

        let x = 5;
        let y = MyBox::new(x);
        assert_eq!(5, x);
        assert_eq!(5, *y);

        let m = MyBox::new(String::from("Hello"));
        // Implicit Deref Coercions, otherwise we have to write this
        // hello(&(*m)[..]);
        hello(&m);
    }

    fn hello(name: &str) {
        println!("{}", name)
    }

}

pub mod c {
    use std::rc::Rc;
    use List::*;

    pub enum List {
        Cons(i32, Rc<List>),
        Nil,
    }

    pub fn run() {
        let a = Rc::new(Cons(5, Rc::new(Cons(6, Rc::new(Nil)))));
        let b = Cons(4, Rc::clone(&a));
        let c = Cons(3, Rc::clone(&a));
    }
}
