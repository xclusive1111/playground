mod point;
mod traits;
use traits::*;

fn main() {
    let mut p1 = point::new_point(5, 10);
    println!("p.x = {}", p1.x());
    p1.move_left(10);
    println!("p.x = {}", p1.x());
    p1.move_right(10);
    println!("px = {}, p.y = {}", p1.x(), p1.y());

    let article = NewsArticle {
        author: String::from("foobar"),
        content: String::from("The quick brown fox jumps over the lazy dog"),
        headline: String::from("Typing legend"),
        location: String::from("Mars"),
    };

    println!("New article available: {}", article.summarize());
    notify(&article);
    notify_2(&article, &article);
    notify_3(&article);
    println!("{}", stringify(&article));

    let s1 = String::from("This is a sentence");
    let result;
    {
        let s2 = String::from("Word");
        result = longest(s1.as_str(), s2.as_str());
        println!("The longest string is '{}'", result);
    }

    // This won't compile because the lifetime of the reference 
    // returned by the `longest` function is the same as the smaller lifetimes 
    // of the references passed in, i.e it's the lifetime of s2.

    // println!("The longest string is '{}'", result);

    let novel = String::from("Read this out loud. You shall pass!");
    let first_sentence;
    {
        // If we move `novel` into this inner scope, this won't compile.

        // let novel = String::from("Read this out loud. You shall pass!");
        first_sentence = novel.split('.').next().expect("Opps! Couldn't find the '.'");
    }
    let tmp = AStruct { content: first_sentence };
    println!("{}", tmp.content);

    let first_str = String::from("First string");
    let second_str = String::from("Second string");
    println!("{}", longest_with_show(first_str.as_str(), second_str.as_str(), &article));
}


fn notify(item: &impl Summary) {
    println!("Breaking news! {}", item.summarize());
}

fn notify_2<T: Summary>(item1: &T, item2: &T) {
    println!("Breaking news! {},\n{}", item1.summarize(), item2.summarize());
}

// Specifying multiple trait bounds with the + syntax
fn notify_3<T: Summary + Show>(item: &T) {
    println!("{}", item.summarize());
    println!("{}", item.show());
}

fn stringify<T>(item: &T) -> String
    where T: Summary + Show 
{
    format!("Summarize: {} - Show: {}", item.summarize(), item.show())
}

// Returning types that implement Traits
fn new_tweet() -> impl Summary + Show {
    Tweet {
        username: String::from("foobar"),
        content: String::from("This is a sample tweet"),
        reply: false,
        retweet: false,
    }
}

// Find the longest string between x & y.
// The returned lifetime is the smaller one between the two.
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

struct AStruct<'b> {
    content: &'b str,
}

fn longest_with_show<'a, T>(x: &'a str, y: &'a str, showable: &T) -> &'a str 
    where T: Show
{
    println!("Showing {}", showable.show());
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
