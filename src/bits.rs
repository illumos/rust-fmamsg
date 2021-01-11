/*
 * Copyright 2020 Oxide Computer Company
 */

#[derive(Debug, Clone)]
pub struct Bits {
    value: u128,
    bitcount: usize,
}

pub fn mask(count: usize) -> u128 {
    let mut mask = 0;
    for i in 0..count {
        mask |= 1 << i;
    }
    mask
}

impl Bits {
    pub fn new() -> Self {
        Bits {
            value: 0,
            bitcount: 0,
        }
    }

    pub fn push(&mut self, bits: u128, count: usize) {
        if count + self.bitcount >= 128 {
            panic!("why cant I hold all these bits");
        }

        let mask = mask(count);

        self.value = ((self.value << count) & !mask) | (mask & bits);
        self.bitcount += count;
    }

    #[allow(dead_code)]
    pub fn dump(&self) {
        println!("bits[{:2}] = {:064b}", self.bitcount, self.value);
        let mut status = "           ".to_string();
        for i in 0..64 {
            let x = 64 - i;
            if x > self.bitcount {
                status += " ";
            } else {
                status += "^";
            }
        }
        println!("{}", status);
    }

    pub fn len(&self) -> usize {
        self.bitcount
    }

    pub fn get_from_left(&self, offset: usize, len: usize) -> u128 {
        let mask = mask(len);

        assert!(offset + len <= self.bitcount);
        let shift = self.bitcount - offset - len;

        (self.value >> shift) & mask
    }

    pub fn new_from_left(&self, offset: usize, len: usize) -> Bits {
        Bits {
            value: self.get_from_left(offset, len),
            bitcount: len,
        }
    }

    #[allow(dead_code)]
    pub fn value(&self) -> u128 {
        self.value
    }
}
