mod front_of_house;

pub use crate::front_of_house::{hosting, serving};

mod back_of_house {
    fn fix_incorrect_order() {
        cook_order();
        super::serve_order();
    }

    fn cook_order() {

    }

    pub struct BreakFast {
        pub toast: String,
        seasonal_fruit: String,
    }

    impl BreakFast {
        pub fn summer(toast: &str) -> BreakFast {
            BreakFast {
                toast: String::from(toast),
                seasonal_fruit: String::from("peaches"),
            }
        }
    }
    
}

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
}

fn serve_order() {
    serving::serve_order();    
}
