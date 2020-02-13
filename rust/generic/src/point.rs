pub struct Point<T, U> {
    x: T,
    y: U,
}
pub fn new_point<T, U>(x: T, y: U) -> Point<T, U> {
    Point{ x, y }
}


impl<T, U> Point<T, U> {
    pub fn x(&self) -> &T {
        &self.x
    }

    pub fn y(&self) -> &U {
        &self.y
    }

}

impl Point<i32, i32> {
    pub fn move_left(&mut self, value :i32) {
        self.x -= value;
    }

    pub fn move_right(&mut self, value :i32) {
        self.x += value;
    }
}
