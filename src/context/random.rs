//! The numbers Lisp's `random` gives.

use std::hash::{BuildHasher, RandomState};

use crate::TulispContext;

/// A SplitMix64 generator, which gives the same numbers on every platform from
/// the same seed.
pub(crate) struct Random {
    state: u64,
}

impl Random {
    /// A generator with a seed from the system, different each time.
    pub(crate) fn from_system() -> Self {
        Self::seeded(RandomState::new().hash_one(()))
    }

    pub(crate) fn seeded(seed: u64) -> Self {
        Self { state: seed }
    }

    /// A generator seeded from TEXT, the same for the same TEXT.
    pub(crate) fn from_text(text: &str) -> Self {
        // FNV-1a, which, unlike the standard library's hashers, gives the same
        // hash in every Rust version.
        let seed = text.bytes().fold(0xcbf2_9ce4_8422_2325, |hash: u64, byte| {
            (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01b3)
        });
        Self::seeded(seed)
    }

    pub(crate) fn next(&mut self) -> u64 {
        self.state = self.state.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.state;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    /// A number from 0 to LIMIT - 1, each as likely as the others. LIMIT is not
    /// 0.
    pub(crate) fn below(&mut self, limit: u64) -> u64 {
        // The numbers below THRESHOLD would make the low results likelier.
        let threshold = limit.wrapping_neg() % limit;
        loop {
            let number = self.next();
            if number >= threshold {
                return number % limit;
            }
        }
    }
}

impl TulispContext {
    /// Seeds Lisp's `random` with SEED, so that it gives the same numbers again
    /// after the same seed, as it does after `(random "TEXT")` with the same
    /// TEXT. A new context has a seed from the system, different each time.
    ///
    /// ```rust
    /// # use tulisp::TulispContext;
    /// let mut ctx = TulispContext::new();
    /// ctx.set_random_seed(42);
    /// let first = ctx.eval_string("(list (random 100) (random 100))").unwrap();
    /// ctx.set_random_seed(42);
    /// let again = ctx.eval_string("(list (random 100) (random 100))").unwrap();
    /// assert!(first.equal(&again));
    /// ```
    pub fn set_random_seed(&mut self, seed: u64) {
        self.random = Random::seeded(seed);
    }
}

#[cfg(test)]
mod tests {
    use super::Random;

    // The generator gives SplitMix64's published numbers.
    #[test]
    fn the_numbers_are_splitmix64s() {
        let mut random = Random::seeded(1234567);
        let numbers: Vec<u64> = (0..3).map(|_| random.next()).collect();
        assert_eq!(
            numbers,
            [
                6457827717110365317,
                3203168211198807973,
                9817491932198370423
            ]
        );
    }

    // With a limit just over 2^63, the numbers below 2^63 - 1 are drawn again:
    // SplitMix64's first two numbers from 1234567 are, and its third is not.
    #[test]
    fn below_draws_again_below_the_threshold() {
        let mut random = Random::seeded(1234567);
        assert_eq!(random.below((1 << 63) + 1), 594119895343594614);
    }

    #[test]
    fn below_stays_below_its_limit() {
        let mut random = Random::seeded(7);
        for limit in [1, 2, 3, 10, u64::MAX] {
            for _ in 0..100 {
                assert!(random.below(limit) < limit);
            }
        }
        let mut seen = [false; 3];
        for _ in 0..100 {
            seen[random.below(3) as usize] = true;
        }
        assert_eq!(seen, [true; 3]);
    }

    #[test]
    fn the_same_text_gives_the_same_numbers() {
        let mut a = Random::from_text("seed");
        let mut b = Random::from_text("seed");
        let mut c = Random::from_text("other");
        let (a, b, c) = (a.next(), b.next(), c.next());
        assert_eq!(a, b);
        assert_ne!(a, c);
    }
}
