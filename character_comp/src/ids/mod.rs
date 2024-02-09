use std::{fmt::Display, iter::Peekable};

use unicode_segmentation::{Graphemes, UnicodeSegmentation};

#[derive(Clone, PartialEq)]
pub enum IDS<C> {
    Character(C),
    LeftToRight(Box<IDS<C>>, Box<IDS<C>>),
    TopToBottom(Box<IDS<C>>, Box<IDS<C>>),
    LeftToMiddleToRight(Box<IDS<C>>, Box<IDS<C>>, Box<IDS<C>>),
    TopToMiddleToBottom(Box<IDS<C>>, Box<IDS<C>>, Box<IDS<C>>),
    SurroundFully(Box<IDS<C>>, Box<IDS<C>>),
    SurroundTopRightLeft(Box<IDS<C>>, Box<IDS<C>>),
    SurroundRightBottomLeft(Box<IDS<C>>, Box<IDS<C>>),
    SurroundTopBottomLeft(Box<IDS<C>>, Box<IDS<C>>),
    SurroundTopLeft(Box<IDS<C>>, Box<IDS<C>>),
    SurroundTopRight(Box<IDS<C>>, Box<IDS<C>>),
    SurroungBottomLeft(Box<IDS<C>>, Box<IDS<C>>),
    Overlap(Box<IDS<C>>, Box<IDS<C>>),
    SurroundTopRightBottom(Box<IDS<C>>, Box<IDS<C>>),
    SurroundRightBottom(Box<IDS<C>>, Box<IDS<C>>),
    ReflectHorizontal(Box<IDS<C>>),
    Rotate(Box<IDS<C>>),
}

impl<C: Display> Display for IDS<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in str_chunks(&|c| format!("{}", c), self) {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

impl<C: std::fmt::Display> std::fmt::Debug for IDS<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

fn str_chunks_many<'a, C, F: Fn(&'a C) -> String>(
    char_str: &'a F,
    c: &'a str,
    cs: Box<[&'a IDS<C>]>,
) -> Box<dyn Iterator<Item = String> + 'a> {
    Box::new(
        std::iter::once(c.to_owned()).chain(
            cs.into_vec()
                .into_iter()
                .flat_map(move |c2| str_chunks(char_str, c2)),
        ),
    )
}

pub fn str_chunks<'a, C, F: Fn(&'a C) -> String>(
    char_str: &'a F,
    ids: &'a IDS<C>,
) -> Box<dyn Iterator<Item = String> + 'a> {
    match ids {
        IDS::Character(c) => Box::new(std::iter::once(char_str(c))),
        IDS::LeftToRight(c1, c2) => str_chunks_many(char_str, "⿰", Box::new([c1, c2])),
        IDS::TopToBottom(c1, c2) => str_chunks_many(char_str, "⿱", Box::new([c1, c2])),
        IDS::LeftToMiddleToRight(c1, c2, c3) => {
            str_chunks_many(char_str, "⿲", Box::new([c1, c2, c3]))
        }
        IDS::TopToMiddleToBottom(c1, c2, c3) => {
            str_chunks_many(char_str, "⿳", Box::new([c1, c2, c3]))
        }
        IDS::SurroundFully(c1, c2) => str_chunks_many(char_str, "⿴", Box::new([c1, c2])),
        IDS::SurroundTopRightLeft(c1, c2) => str_chunks_many(char_str, "⿵", Box::new([c1, c2])),
        IDS::SurroundRightBottomLeft(c1, c2) => str_chunks_many(char_str, "⿶", Box::new([c1, c2])),
        IDS::SurroundTopBottomLeft(c1, c2) => str_chunks_many(char_str, "⿷", Box::new([c1, c2])),
        IDS::SurroundTopLeft(c1, c2) => str_chunks_many(char_str, "⿸", Box::new([c1, c2])),
        IDS::SurroundTopRight(c1, c2) => str_chunks_many(char_str, "⿹", Box::new([c1, c2])),
        IDS::SurroungBottomLeft(c1, c2) => str_chunks_many(char_str, "⿺", Box::new([c1, c2])),
        IDS::Overlap(c1, c2) => str_chunks_many(char_str, "⿻", Box::new([c1, c2])),
        IDS::SurroundTopRightBottom(c1, c2) => str_chunks_many(char_str, "⿼", Box::new([c1, c2])),
        IDS::SurroundRightBottom(c1, c2) => str_chunks_many(char_str, "⿽", Box::new([c1, c2])),
        IDS::ReflectHorizontal(c) => str_chunks_many(char_str, "⿾", Box::new([c])),
        IDS::Rotate(c) => str_chunks_many(char_str, "⿿", Box::new([c])),
    }
}

fn parse_one_arg<
    'a,
    C,
    I: Iterator<Item = &'a str>,
    F: Fn(Peekable<I>) -> Option<(Peekable<I>, C)>,
>(
    parse_char: &F,
    mut glyphs: Peekable<I>,
    wrap: fn(Box<IDS<C>>) -> IDS<C>,
) -> Option<(Peekable<I>, IDS<C>)> {
    glyphs.next()?;
    let (glyphs, c) = parse_ids(parse_char, glyphs)?;
    Some((glyphs, wrap(Box::new(c))))
}

fn parse_two_args<
    'a,
    C,
    I: Iterator<Item = &'a str>,
    F: Fn(Peekable<I>) -> Option<(Peekable<I>, C)>,
>(
    parse_char: &F,
    mut glyphs: Peekable<I>,
    compose: fn(Box<IDS<C>>, Box<IDS<C>>) -> IDS<C>,
) -> Option<(Peekable<I>, IDS<C>)> {
    glyphs.next()?;
    let (glyphs, c1) = parse_ids(parse_char, glyphs)?;
    let (glyphs, c2) = parse_ids(parse_char, glyphs)?;
    Some((glyphs, compose(Box::new(c1), Box::new(c2))))
}

fn parse_three_args<
    'a,
    C,
    I: Iterator<Item = &'a str>,
    F: Fn(Peekable<I>) -> Option<(Peekable<I>, C)>,
>(
    parse_char: &F,
    mut glyphs: Peekable<I>,
    compose: fn(Box<IDS<C>>, Box<IDS<C>>, Box<IDS<C>>) -> IDS<C>,
) -> Option<(Peekable<I>, IDS<C>)> {
    glyphs.next()?;
    let (glyphs, c1) = parse_ids(parse_char, glyphs)?;
    let (glyphs, c2) = parse_ids(parse_char, glyphs)?;
    let (glyphs, c3) = parse_ids(parse_char, glyphs)?;
    Some((glyphs, compose(Box::new(c1), Box::new(c2), Box::new(c3))))
}

pub fn parse_ids<
    'a,
    C,
    I: Iterator<Item = &'a str>,
    F: Fn(Peekable<I>) -> Option<(Peekable<I>, C)>,
>(
    parse_char: &F,
    mut glyphs: Peekable<I>,
) -> Option<(Peekable<I>, IDS<C>)> {
    let peeked = glyphs.peek()?;
    match *peeked {
        "⿰" => parse_two_args(parse_char, glyphs, IDS::LeftToRight),
        "⿱" => parse_two_args(parse_char, glyphs, IDS::TopToBottom),
        "⿲" => parse_three_args(parse_char, glyphs, IDS::LeftToMiddleToRight),
        "⿳" => parse_three_args(parse_char, glyphs, IDS::TopToMiddleToBottom),
        "⿴" => parse_two_args(parse_char, glyphs, IDS::SurroundFully),
        "⿵" => parse_two_args(parse_char, glyphs, IDS::SurroundTopRightLeft),
        "⿶" => parse_two_args(parse_char, glyphs, IDS::SurroundRightBottomLeft),
        "⿷" => parse_two_args(parse_char, glyphs, IDS::SurroundTopBottomLeft),
        "⿸" => parse_two_args(parse_char, glyphs, IDS::SurroundTopLeft),
        "⿹" => parse_two_args(parse_char, glyphs, IDS::SurroundTopRight),
        "⿺" => parse_two_args(parse_char, glyphs, IDS::SurroungBottomLeft),
        "⿻" => parse_two_args(parse_char, glyphs, IDS::Overlap),
        "⿼" => parse_two_args(parse_char, glyphs, IDS::SurroundTopRightBottom),
        "⿽" => parse_two_args(parse_char, glyphs, IDS::SurroundRightBottom),
        "⿾" => parse_one_arg(parse_char, glyphs, IDS::ReflectHorizontal),
        "⿿" => parse_one_arg(parse_char, glyphs, IDS::Rotate),
        _ => {
            let (glyphs, c) = parse_char(glyphs)?;
            Some((glyphs, IDS::Character(c)))
        }
    }
}

/// Parse *entire* `&str` as IDS sequence
pub fn parse_ids_str<
    'a,
    C,
    F: Fn(Peekable<Graphemes<'a>>) -> Option<(Peekable<Graphemes<'a>>, C)>,
>(
    parse_char: &F,
    s: &'a str,
) -> Option<IDS<C>> {
    let i = s.graphemes(true).peekable();
    let (mut i, ids) = parse_ids(parse_char, i)?;
    match i.next() {
        None => Some(ids),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::iter::Peekable;

    use super::{parse_ids_str, IDS};
    use quickcheck::{Arbitrary, QuickCheck};
    use unicode_segmentation::Graphemes;

    #[derive(Clone, PartialEq)]
    struct Char(String);

    impl std::fmt::Display for Char {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl Arbitrary for Char {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            match bool::arbitrary(g) {
                true => Char("你".to_owned()),
                false => Char("好".to_owned()),
            }
        }
    }

    impl<C: Arbitrary> Arbitrary for IDS<C> {
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
                _ => IDS::Character(C::arbitrary(g)),
            }
        }
    }

    #[test]
    fn test_print_then_parse() {
        fn print_then_parse(ids: IDS<Char>) -> () {
            let s = ids.to_string();
            let ids2 = parse_ids_str(
                &|mut i: Peekable<Graphemes>| {
                    let c = Char(i.next()?.to_owned());
                    Some((i, c))
                },
                s.as_str(),
            )
            .unwrap_or_else(|| panic!("Failed to parse: '{}'", s));
            assert_eq!(ids, ids2);
        }
        const TEST_COUNT: u64 = 10_000;
        QuickCheck::new()
            .tests(TEST_COUNT)
            .min_tests_passed(TEST_COUNT)
            .max_tests(TEST_COUNT)
            .quickcheck(print_then_parse as fn(ids: IDS<Char>) -> ());
    }
}
