/*
 * Copyright 2011 Joshua M. Clulow <josh@sysmgr.org>
 * Copyright 2020 Oxide Computer Company
 */

use std::fmt;

mod bits;
use bits::Bits;

mod crc;
use crc::crc;

pub type Result<T> = std::result::Result<T, DiagCodeError>;

#[derive(Debug, Clone)]
pub struct DiagCodeError {
    msg: String,
}

impl fmt::Display for DiagCodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

fn err<T>(msg: &str) -> Result<T> {
    Err(DiagCodeError {
        msg: msg.to_string(),
    })
}

/*
 * The FMA diagnostic code alphabet does not include letters that are easy to
 * conflate with numbers in some fonts; e.g., zero but not "O", 1 but not "I",
 * etc.
 */
static ALPHABET: &'static [char] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'C', 'D', 'E', 'F',
    'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y',
];

fn to_bits(c: char) -> Option<u128> {
    ALPHABET.iter().position(|a| *a == c).map(|i| i as u128)
}

struct CodeForm {
    cap: u128,
    codelenbits: u128,
    datalen: usize,
    csumlen: usize,
}

/*
 * There are four diagnostic code forms.  The shortest form that will fit the
 * entry number is always chosen.
 */
static FORMS: &'static [CodeForm] = &[
    CodeForm {
        /*
         * DICT-XXXX-XX
         */
        cap: 2097152,
        codelenbits: 0b00,
        datalen: 21,
        csumlen: 5,
    },
    CodeForm {
        /*
         * DICT-XXXX-XXXX-XX
         */
        cap: 274880004096,
        codelenbits: 0b01,
        datalen: 38,
        csumlen: 8,
    },
    CodeForm {
        /*
         * DICT-XXXX-XXXX-XXXX-XX
         */
        cap: 36029071898968064,
        codelenbits: 0b10,
        datalen: 55,
        csumlen: 11,
    },
    CodeForm {
        /*
         * DICT-XXXX-XXXX-XXXX-XXXX-XX
         */
        cap: 4722402511941544181759,
        codelenbits: 0b11,
        datalen: 72,
        csumlen: 14,
    },
];

impl CodeForm {
    fn for_entry(entry: u128) -> Result<&'static CodeForm> {
        for f in FORMS.iter() {
            if entry < f.cap {
                return Ok(f);
            }
        }

        err(&format!("entry {} is too large to encode", entry))
    }

    fn for_length(bits: usize) -> Result<&'static CodeForm> {
        for f in FORMS.iter() {
            if bits == 4 + f.datalen + f.csumlen {
                return Ok(f);
            }
        }

        err(&format!("unexpected data length of {} bits", bits))
    }

    fn entry_valid(entry: u128) -> bool {
        for f in FORMS.iter() {
            if entry < f.cap {
                return true;
            }
        }
        false
    }
}

fn checksum(dict: &str, mut bits: Bits, nsumbits: usize) -> u128 {
    bits.push(0, nsumbits);

    let mut csum = 0;
    for b in dict.bytes() {
        csum = crc(csum, b as u128);
    }

    for i in 0..(bits.len() / 5) {
        let bi = bits.get_from_left(i * 5, 5);
        csum = crc(csum, bi);
    }

    csum & bits::mask(nsumbits)
}

/*
 * Dictionary names must be composed of uppercase ASCII letters and ASCII
 * numbers.
 */
fn dict_char(c: char) -> bool {
    c.is_ascii_uppercase() || c.is_ascii_digit()
}

#[derive(PartialEq)]
enum State {
    Dict,
    Group,
}

/**
 * This object represents a complete and valid diagnostic code.  It is
 * constructed from its component parts with `new()`, or parsed from the string
 * form with `parse()`.  It may be rendered as a string with `encode()`, or the
 * `dictionary()` and `entry()` methods may be used to extract individual
 * values.
 */
#[derive(Clone, PartialEq, Debug)]
pub struct DiagCode {
    dictionary: String,
    entry: u128,
}

impl DiagCode {
    pub fn new(dictionary: &str, entry: u128) -> Result<DiagCode> {
        if dictionary.is_empty() {
            err("dictionary name must not be empty")
        } else if !dictionary.chars().all(dict_char) {
            err("dictionary name must be entirely ASCII letters and numbers")
        } else if !CodeForm::entry_valid(entry) {
            err("entry value is too large to be encoded")
        } else {
            Ok(DiagCode {
                dictionary: dictionary.to_string(),
                entry,
            })
        }
    }

    pub fn dictionary(&self) -> &str {
        &self.dictionary
    }

    pub fn entry(&self) -> u128 {
        self.entry
    }

    pub fn parse(code: &str) -> Result<DiagCode> {
        let mut s = State::Dict;
        let mut dict = String::new();
        let mut g = 0;
        let mut gc = 0;
        let mut bits = Bits::new();

        for c in code.chars() {
            match s {
                State::Dict => {
                    if dict_char(c) {
                        dict.push(c);
                    } else if c == '-' && !dict.is_empty() {
                        s = State::Group;
                        gc = 0;
                    } else {
                        return err("malformed code");
                    }
                }
                State::Group => {
                    if gc > 4 {
                        /*
                         * No group may have more than four characters.
                         */
                        return err("malformed code");
                    }

                    if c == '-' {
                        if gc != 4 {
                            /*
                             * Only the last group may have less than four
                             * characters.
                             */
                            return err("malformed code");
                        }
                        gc = 0;

                        g += 1;
                        if g > 5 {
                            /*
                             * The longest possible code can have but five
                             * groups.
                             */
                            return err("malformed code");
                        }
                        continue;
                    }

                    if let Some(i) = to_bits(c) {
                        bits.push(i, 5);
                        gc += 1;
                    } else {
                        return err("malformed code");
                    }
                }
            }
        }

        if s != State::Group && gc != 2 {
            /*
             * The code must end with a group of 2 characters.
             */
            return err("malformed code");
        }

        if bits.get_from_left(0, 2) != 0b01u128 {
            return err("unexpected type value");
        }

        let mf = CodeForm::for_length(bits.len())?;

        if bits.get_from_left(2, 2) != mf.codelenbits {
            return err("incorrect size value");
        }

        /*
         * The checksum must include the size, type, and data bits:
         */
        let csumtarget = bits.new_from_left(0, 4 + mf.datalen);
        let expected = checksum(&dict, csumtarget, mf.csumlen);
        let actual = bits.get_from_left(4 + mf.datalen, mf.csumlen);

        if expected != actual {
            return err("checksum mismatch");
        }

        Ok(DiagCode {
            dictionary: dict,
            entry: bits.get_from_left(4, mf.datalen),
        })
    }

    pub fn encode(&self) -> String {
        let mf = CodeForm::for_entry(self.entry).unwrap();

        let mut bits = Bits::new();

        /*
         * Code type (2 bit).  There is presently only one defined code type.
         */
        bits.push(0b01, 2);

        /*
         * Code size (2 bit):
         */
        bits.push(mf.codelenbits, 2);

        /*
         * Data bits:
         */
        bits.push(self.entry, mf.datalen);

        /*
         * Checksum bits:
         */
        let sumbits = checksum(&self.dictionary, bits.clone(), mf.csumlen);
        bits.push(sumbits, mf.csumlen);

        /*
         * Produce the final format string; e.g., "ZFS-8000-14".
         */
        let mut alphastr = self.dictionary.clone();
        for i in 0..(bits.len() / 5) {
            if i % 4 == 0 {
                /*
                 * Each group of four digits should be separated by a dash:
                 */
                alphastr.push('-');
            }

            /*
             * Each group of five bits is rendered as one character from the
             * diagcode alphabet:
             */
            alphastr.push(ALPHABET[bits.get_from_left(i * 5, 5) as usize]);
        }

        alphastr
    }
}

#[cfg(test)]
mod tests {
    use super::DiagCode;

    #[derive(Debug)]
    struct TestCase {
        msgid: String,
        obj: DiagCode,
    }

    impl TestCase {
        fn new(msgid: &str, dictionary: &str, entry: u128) -> TestCase {
            TestCase {
                msgid: msgid.to_string(),
                obj: DiagCode {
                    dictionary: dictionary.to_string(),
                    entry,
                },
            }
        }
    }

    fn gencases() -> Vec<TestCase> {
        vec![
            TestCase::new("ZFS-8000-14", "ZFS", 1),
            TestCase::new("ZFS-8000-2Q", "ZFS", 2),
            TestCase::new("ZFS-8000-3C", "ZFS", 3),
            TestCase::new("ZFS-8000-4J", "ZFS", 4),
            TestCase::new("ZFS-8000-5E", "ZFS", 5),
            TestCase::new("ZFS-8000-6X", "ZFS", 6),
            TestCase::new("ZFS-8000-72", "ZFS", 7),
            TestCase::new("ZFS-8000-8A", "ZFS", 8),
            TestCase::new("ZFS-8000-9P", "ZFS", 9),
            TestCase::new("ZFS-8000-A5", "ZFS", 10),
            TestCase::new("ZFS-8000-CS", "ZFS", 11),
            TestCase::new("ZFS-8000-D3", "ZFS", 12),
            TestCase::new("ZFS-8000-EY", "ZFS", 13),
            TestCase::new("ZFS-8000-FD", "ZFS", 14),
            TestCase::new("ZFS-8000-GH", "ZFS", 15),
            TestCase::new("ZFS-8000-HC", "ZFS", 16),
            TestCase::new("ZFS-8000-JQ", "ZFS", 17),
            TestCase::new("ZFS-8000-K4", "ZFS", 18),
            TestCase::new("SENSOR-8000-09", "SENSOR", 0),
            TestCase::new("SENSOR-8000-1N", "SENSOR", 1),
            TestCase::new("SENSOR-8000-26", "SENSOR", 2),
            TestCase::new("SENSOR-8000-3T", "SENSOR", 3),
            TestCase::new("SENSOR-8000-40", "SENSOR", 4),
            TestCase::new("SENSOR-8000-5V", "SENSOR", 5),
            TestCase::new("SENSOR-8000-6G", "SENSOR", 6),
            TestCase::new("SENSOR-8000-7L", "SENSOR", 7),
            TestCase::new("SCA1000-8000-10", "SCA1000", 1),
        ]
    }

    #[test]
    fn encode_success() {
        for g in gencases().iter() {
            println!("testcase {:?}", g);
            let diagcode =
                DiagCode::new(&g.obj.dictionary, g.obj.entry).unwrap();
            let out = diagcode.encode();
            assert_eq!(&out, &g.msgid);
        }
    }

    #[test]
    fn decode_success() {
        for g in gencases().iter() {
            println!("testcase {:?}", g);
            let out = DiagCode::parse(&g.msgid).expect("parse");
            assert_eq!(&out, &g.obj);
        }
    }

    #[test]
    fn decode_fail_checksum_error() {
        let out = DiagCode::parse("ZFS-8000-15");
        assert!(out.is_err());
    }

    #[test]
    fn decode_fail_malformed_code() {
        let out = DiagCode::parse("ZFS-8000-140");
        assert!(out.is_err());
    }

    #[test]
    fn create_lowercase() {
        let out = DiagCode::new("zfs", 80);
        assert!(out.is_err());
    }

    #[test]
    fn create_uppercase() {
        let out = DiagCode::new("ZFS", 80);
        assert!(out.is_ok());
    }

    #[test]
    fn too_large() {
        let out = DiagCode::new("ZFS", 4722402511941544181759);
        assert!(out.is_err());
    }

    #[test]
    fn empty_dictionary() {
        let out = DiagCode::new("", 100);
        assert!(out.is_err());
    }
}
