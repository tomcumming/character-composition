use std::{borrow::Cow, fmt::Display, iter::Peekable};

use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone, PartialEq)]
pub enum IDS {
    Character(String),
    Code(String),
    LeftToRight(Box<IDS>, Box<IDS>),
    TopToBottom(Box<IDS>, Box<IDS>),
    LeftToMiddleToRight(Box<IDS>, Box<IDS>, Box<IDS>),
    TopToMiddleToBottom(Box<IDS>, Box<IDS>, Box<IDS>),
    SurroundFully(Box<IDS>, Box<IDS>),
    SurroundTopRightLeft(Box<IDS>, Box<IDS>),
    SurroundRightBottomLeft(Box<IDS>, Box<IDS>),
    SurroundTopBottomLeft(Box<IDS>, Box<IDS>),
    SurroundTopLeft(Box<IDS>, Box<IDS>),
    SurroundTopRight(Box<IDS>, Box<IDS>),
    SurroungBottomLeft(Box<IDS>, Box<IDS>),
    Overlap(Box<IDS>, Box<IDS>),
    SurroundTopRightBottom(Box<IDS>, Box<IDS>),
    SurroundRightBottom(Box<IDS>, Box<IDS>),
    ReflectHorizontal(Box<IDS>),
    Rotate(Box<IDS>),
}

impl Display for IDS {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in str_chunks(self) {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for IDS {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

fn str_chunks_many<'a>(c: &'a str, cs: Box<[&'a IDS]>) -> Box<dyn Iterator<Item = String> + 'a> {
    Box::new(
        std::iter::once(c.to_owned())
            .chain(cs.into_vec().into_iter().flat_map(move |c2| str_chunks(c2))),
    )
}

pub fn str_chunks<'a>(ids: &'a IDS) -> Box<dyn Iterator<Item = String> + 'a> {
    match ids {
        IDS::Character(c) => Box::new(std::iter::once(c.to_owned())),
        IDS::Code(c) => Box::new(std::iter::once(c.to_owned())),
        IDS::LeftToRight(c1, c2) => str_chunks_many("⿰", Box::new([c1, c2])),
        IDS::TopToBottom(c1, c2) => str_chunks_many("⿱", Box::new([c1, c2])),
        IDS::LeftToMiddleToRight(c1, c2, c3) => str_chunks_many("⿲", Box::new([c1, c2, c3])),
        IDS::TopToMiddleToBottom(c1, c2, c3) => str_chunks_many("⿳", Box::new([c1, c2, c3])),
        IDS::SurroundFully(c1, c2) => str_chunks_many("⿴", Box::new([c1, c2])),
        IDS::SurroundTopRightLeft(c1, c2) => str_chunks_many("⿵", Box::new([c1, c2])),
        IDS::SurroundRightBottomLeft(c1, c2) => str_chunks_many("⿶", Box::new([c1, c2])),
        IDS::SurroundTopBottomLeft(c1, c2) => str_chunks_many("⿷", Box::new([c1, c2])),
        IDS::SurroundTopLeft(c1, c2) => str_chunks_many("⿸", Box::new([c1, c2])),
        IDS::SurroundTopRight(c1, c2) => str_chunks_many("⿹", Box::new([c1, c2])),
        IDS::SurroungBottomLeft(c1, c2) => str_chunks_many("⿺", Box::new([c1, c2])),
        IDS::Overlap(c1, c2) => str_chunks_many("⿻", Box::new([c1, c2])),
        IDS::SurroundTopRightBottom(c1, c2) => str_chunks_many("⿼", Box::new([c1, c2])),
        IDS::SurroundRightBottom(c1, c2) => str_chunks_many("⿽", Box::new([c1, c2])),
        IDS::ReflectHorizontal(c) => str_chunks_many("⿾", Box::new([c])),
        IDS::Rotate(c) => str_chunks_many("⿿", Box::new([c])),
    }
}

fn parse_code<'a, I: Iterator<Item = &'a str>>(
    mut glyphs: Peekable<I>,
) -> Option<(Peekable<I>, String)> {
    let mut s = String::new();
    loop {
        let c = glyphs.next()?;
        if c == ";" {
            break;
        } else {
            s += c;
        }
    }
    Some((glyphs, s))
}

fn parse_one_arg<'a, I: Iterator<Item = &'a str>>(
    glyphs: Peekable<I>,
    wrap: fn(Box<IDS>) -> IDS,
) -> Option<(Peekable<I>, IDS)> {
    let (glyphs, c) = parse_ids(glyphs)?;
    Some((glyphs, wrap(Box::new(c))))
}

fn parse_two_args<'a, I: Iterator<Item = &'a str>>(
    glyphs: Peekable<I>,
    compose: fn(Box<IDS>, Box<IDS>) -> IDS,
) -> Option<(Peekable<I>, IDS)> {
    let (glyphs, c1) = parse_ids(glyphs)?;
    let (glyphs, c2) = parse_ids(glyphs)?;
    Some((glyphs, compose(Box::new(c1), Box::new(c2))))
}

fn parse_three_args<'a, I: Iterator<Item = &'a str>>(
    glyphs: Peekable<I>,
    compose: fn(Box<IDS>, Box<IDS>, Box<IDS>) -> IDS,
) -> Option<(Peekable<I>, IDS)> {
    let (glyphs, c1) = parse_ids(glyphs)?;
    let (glyphs, c2) = parse_ids(glyphs)?;
    let (glyphs, c3) = parse_ids(glyphs)?;
    Some((glyphs, compose(Box::new(c1), Box::new(c2), Box::new(c3))))
}

fn try_combiner_from_code(code: &str) -> Option<String> {
    // TODO what encoding is this?
    let c_code = code
        .strip_prefix("U-i001+2FF")
        .or_else(|| code.strip_prefix("U-i002+2FF"))?;
    match c_code {
        "0" => Some("⿰".to_owned()),
        "1" => Some("⿱".to_owned()),
        "2" => Some("⿲".to_owned()),
        "3" => Some("⿳".to_owned()),
        "4" => Some("⿴".to_owned()),
        "5" => Some("⿵".to_owned()),
        "6" => Some("⿶".to_owned()),
        "7" => Some("⿷".to_owned()),
        "8" => Some("⿸".to_owned()),
        "9" => Some("⿹".to_owned()),
        "A" => Some("⿺".to_owned()),
        "B" => Some("⿻".to_owned()),
        "C" => Some("⿼".to_owned()),
        "D" => Some("⿽".to_owned()),
        "E" => Some("⿾".to_owned()),
        "F" => Some("⿿".to_owned()),
        _ => None,
    }
}

pub fn parse_ids<'a, I: Iterator<Item = &'a str>>(
    mut glyphs: Peekable<I>,
) -> Option<(Peekable<I>, IDS)> {
    let c = Cow::Borrowed(glyphs.next()?);

    let (glyphs, c) = if c == "&" {
        let (glyphs, code) = parse_code(glyphs)?;
        match try_combiner_from_code(code.as_str()) {
            Some(combiner) => (glyphs, Cow::Owned(combiner)),
            None => {
                return Some((glyphs, IDS::Code(code)));
            }
        }
    } else {
        (glyphs, c)
    };

    match c.as_ref() {
        "⿰" => parse_two_args(glyphs, IDS::LeftToRight),
        "⿱" => parse_two_args(glyphs, IDS::TopToBottom),
        "⿲" => parse_three_args(glyphs, IDS::LeftToMiddleToRight),
        "⿳" => parse_three_args(glyphs, IDS::TopToMiddleToBottom),
        "⿴" => parse_two_args(glyphs, IDS::SurroundFully),
        "⿵" => parse_two_args(glyphs, IDS::SurroundTopRightLeft),
        "⿶" => parse_two_args(glyphs, IDS::SurroundRightBottomLeft),
        "⿷" => parse_two_args(glyphs, IDS::SurroundTopBottomLeft),
        "⿸" => parse_two_args(glyphs, IDS::SurroundTopLeft),
        "⿹" => parse_two_args(glyphs, IDS::SurroundTopRight),
        "⿺" => parse_two_args(glyphs, IDS::SurroungBottomLeft),
        "⿻" => parse_two_args(glyphs, IDS::Overlap),
        "⿼" => parse_two_args(glyphs, IDS::SurroundTopRightBottom),
        "⿽" => parse_two_args(glyphs, IDS::SurroundRightBottom),
        "⿾" => parse_one_arg(glyphs, IDS::ReflectHorizontal),
        "⿿" => parse_one_arg(glyphs, IDS::Rotate),
        _ => Some((glyphs, IDS::Character(c.into_owned()))),
    }
}

/// Parse *entire* `&str` as IDS sequence
pub fn parse_ids_str<'a>(s: &'a str) -> Option<IDS> {
    let i = s.graphemes(true).peekable();
    let (mut i, ids) = parse_ids(i)?;
    match i.next() {
        None => Some(ids),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_ids_str, IDS};
    use quickcheck::{Arbitrary, QuickCheck};

    impl Arbitrary for IDS {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let growing = 16;
            match u32::arbitrary(g) % (growing * 3) {
                0 => IDS::LeftToRight(Box::new(IDS::arbitrary(g)), Box::new(IDS::arbitrary(g))),
                1 => IDS::TopToBottom(Box::new(IDS::arbitrary(g)), Box::new(IDS::arbitrary(g))),
                2 => IDS::LeftToMiddleToRight(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                3 => IDS::TopToMiddleToBottom(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                4 => IDS::SurroundFully(Box::new(IDS::arbitrary(g)), Box::new(IDS::arbitrary(g))),
                5 => IDS::SurroundTopRightLeft(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                6 => IDS::SurroundRightBottomLeft(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                7 => IDS::SurroundTopBottomLeft(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                8 => IDS::SurroundTopLeft(Box::new(IDS::arbitrary(g)), Box::new(IDS::arbitrary(g))),
                9 => {
                    IDS::SurroundTopRight(Box::new(IDS::arbitrary(g)), Box::new(IDS::arbitrary(g)))
                }
                10 => IDS::SurroungBottomLeft(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                11 => IDS::Overlap(Box::new(IDS::arbitrary(g)), Box::new(IDS::arbitrary(g))),
                12 => IDS::SurroundTopRightBottom(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                13 => IDS::SurroundRightBottom(
                    Box::new(IDS::arbitrary(g)),
                    Box::new(IDS::arbitrary(g)),
                ),
                14 => IDS::ReflectHorizontal(Box::new(IDS::arbitrary(g))),
                15 => IDS::Rotate(Box::new(IDS::arbitrary(g))),
                n => IDS::Character(if n % 2 == 0 { "你" } else { "好" }.to_owned()),
            }
        }
    }

    #[test]
    fn test_print_then_parse() {
        fn print_then_parse(ids: IDS) -> () {
            let s = ids.to_string();
            let ids2 =
                parse_ids_str(s.as_str()).unwrap_or_else(|| panic!("Failed to parse: '{}'", s));
            assert_eq!(ids, ids2);
        }
        const TEST_COUNT: u64 = 10_000;
        QuickCheck::new()
            .tests(TEST_COUNT)
            .min_tests_passed(TEST_COUNT)
            .max_tests(TEST_COUNT)
            .quickcheck(print_then_parse as fn(ids: IDS) -> ());
    }
}
