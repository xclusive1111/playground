pub fn add_two(a: i32) -> i32 {
    internal_adder(a, 2)
}

fn internal_adder(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn this_should_fail() {
        panic!("This test will fail deliberately");
    }

    #[test]
    fn another() -> Result<(), String> {
        if 2 + 2 == 4 {
            Ok(())
        } else {
            Err(String::from("2 + 2 =/= 4"))
        }
    }

    // This test is ignored
    #[test]
    #[ignore]
    fn for_fun() {
        panic!("This never happens")
    }

    #[test]
    fn test_add_two() {
        let n = add_two(10);
        assert_eq!(n, 12);
    }

    // Test a private function.
    #[test]
    fn test_internal_adder() {
        assert_eq!(12, internal_adder(10, 2));
    }
}
