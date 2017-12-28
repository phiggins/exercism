pub fn nth(n: usize) -> Result<i32, &'static str> {
    if n < 1 {
        return Err("param must be 1 or more")
    }

    let mut primes = vec![2];
    let mut current = 3;

    while primes.len() < n {
        if !primes.iter().any(|prime| current % prime == 0)  {
            primes.push(current);
        }

        current += 2
    }

    Ok(primes.pop().expect("wat"))
}
