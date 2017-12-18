pub fn raindrops(n: usize) -> String {
    let mut s = "".to_string();

    if n % 3 == 0 {
        s = s + "Pling";
    }

    if n % 5 == 0 {
        s = s + "Plang";
    }

    if n % 7 == 0 {
        s = s + "Plong";
    }

    if s == "" {
        s = s + &n.to_string();
    }

    s
}
