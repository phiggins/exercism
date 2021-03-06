extern crate nth_prime as np;

#[test]
fn test_first_prime() {
    assert_eq!(np::nth(1), Ok(2));
}

#[test]
fn test_second_prime() {
    assert_eq!(np::nth(2), Ok(3));
}

#[test]
fn test_fifth_prime() {
    assert_eq!(np::nth(5), Ok(11));
}

#[test]
fn test_sixth_prime() {
    assert_eq!(np::nth(6), Ok(13));
}

#[test]
fn test_seventh_prime() {
    assert_eq!(np::nth(7), Ok(17));
}

#[test]
fn test_big_prime() {
    assert_eq!(np::nth(10001), Ok(104743));
}

#[test]
fn test_rilly_big_prime() {
    assert_eq!(np::nth(100_001), Ok(1299721));
}

#[test]
fn test_zeroth_prime() {
    assert!(np::nth(0).is_err());
}
