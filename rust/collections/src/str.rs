pub fn tic_tac_toe() -> String {
    let s1 = String::from("tic");
    let s2 = String::from("tac");
    let s3 = String::from("toe");
    let s = s1 + "-" + &s2 + "-" + &s3;

    let mut str1 = String::from("foo");
    let str2 = String::from("bar");
    str1.push_str(" bar");
    println!("{}", str1);

    // Not compile
    // println!("{}", s1);

    // Cannot use s1 here because it's already moved
    let s = format!("{}-{}-{}", "tic", s2, s3);
    s
}
