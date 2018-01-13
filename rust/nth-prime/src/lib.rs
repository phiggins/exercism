// Find the nth prime with an implementation of the Sieve of Erasthones:
// https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
pub fn nth(n: usize) -> Result<usize, &'static str> {
    if n < 1 {
        return Err("param must be 1 or more")
    }

    let upper_bound = upper_bound(n) as usize;

    let mut sieve = vec![true; upper_bound];
    let mut primes = vec![];

    for prime in 2..upper_bound {
        if sieve[prime] == false {
            continue;
        }

        primes.push(prime);

        let mut i = prime*prime;
        while i < upper_bound {
            sieve[i] = false;
            i += prime;
        }
    }

    Ok(primes[n-1])
}

// Get the upper bound of the nth prime number:
// https://en.wikipedia.org/wiki/Prime_number_theorem#Approximations_for_the_nth_prime_number
fn upper_bound(n: usize) -> usize {
    if n < 6 {
        return 37;
    }

    let n = n as f64;
    (n * (n.ln() + n.ln().ln())).ceil() as usize
}
