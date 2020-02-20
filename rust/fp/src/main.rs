mod smart_pointers;
mod concurrency;

use smart_pointers::b;
use smart_pointers::c;
use concurrency::d;
use concurrency::e;
use concurrency::f;

fn main() {
    b::run();
    c::run();
    d::run();
    e::run();
    f::run();
}
