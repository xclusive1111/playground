use std::collections::HashMap;
pub fn new_hashmap() -> HashMap<String, u32> {
    let mut scores = HashMap::new();
    scores.insert(String::from("foo"), 20);
    scores.insert(String::from("bar"), 45);
    println!("{:?}", scores);

    let teams = vec![String::from("Red"), String::from("Blue")];
    let initial_scores = vec![10, 20];

    let sc: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();
    println!("{:?}", sc);
    scores
}
