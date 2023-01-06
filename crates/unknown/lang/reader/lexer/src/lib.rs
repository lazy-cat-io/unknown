use std::mem;

use logos::Logos;
use text_size::TextSize;

use unknown_syntax::TokenKind;
use unknown_token::Tokens;

#[derive(PartialEq, Logos)]
enum LexerTokenKind {
    #[token("nil")]
    Nil,

    #[regex(r#"true|false"#)]
    Boolean,

    #[regex(r#"-?[0-9]+"#)]
    Integer,

    #[regex(r#"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+"#)]
    Float,

    #[regex(r#"([-+]?[0-9]+)/([0-9]+)"#)]
    Ratio,

    #[regex("[a-zA-Z_]+[/a-zA-Z0-9_]*")]
    Symbol,

    #[regex(":[a-zA-Z_]+[/a-zA-Z0-9_]*")]
    Keyword,

    // TODO: [2022-12-27, Ilshat Sultanov] handle other whitespace characters
    #[regex(r#"[ ,\n]+"#)]
    Whitespace,

    #[token("#")]
    Hash,

    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    _SingleQuote,
    _DoubleQuote,
    _Escape,
    _StringContent,

    #[regex(r#""([^"\\\n]|\\.)*"?"#)]
    __String,

    _CommentContent,

    #[regex(r#";;;;.*"#)]
    __CommentHeader,

    #[regex(r#";;;.*"#)]
    __CommentDefinition,

    #[regex(r#";;.*"#)]
    __CommentForm,

    #[regex(r#";.*"#)]
    __CommentLine,

    #[error]
    Error,
}

pub fn lex(text: &str) -> Tokens {
    let mut kinds = Vec::new();
    let mut starts = Vec::new();

    let mut lexer = LexerTokenKind::lexer(text);
    while let Some(kind) = lexer.next() {
        let range = lexer.span();
        let start = (range.start as u32).into();

        let mut handler = |k, s| {
            kinds.push(k);
            starts.push(s);
        };

        match kind {
            LexerTokenKind::__String => lex_string(lexer.slice(), start, handler),
            LexerTokenKind::__CommentHeader => lex_comment_header(start, range.len(), handler),
            LexerTokenKind::__CommentDefinition => {
                lex_comment_definition(start, range.len(), handler)
            }
            LexerTokenKind::__CommentForm => lex_comment_form(start, range.len(), handler),
            LexerTokenKind::__CommentLine => lex_comment_line(start, range.len(), handler),
            _ => handler(unsafe { mem::transmute(kind) }, start),
        }
    }

    starts.push((text.len() as u32).into());

    kinds.shrink_to_fit();
    starts.shrink_to_fit();

    Tokens::new(kinds, starts)
}

fn lex_string(s: &str, offset: TextSize, mut f: impl FnMut(TokenKind, TextSize)) {
    #[derive(Clone, Copy)]
    enum Mode {
        StartContent,
        InternalContent,
        Escape,
    }

    let mut mode = Mode::InternalContent;
    let mut pos = offset;

    for c in s.chars() {
        match (mode, c) {
            (Mode::InternalContent | Mode::StartContent, '"') => {
                mode = Mode::StartContent;
                f(TokenKind::DoubleQuote, pos);
            }
            (Mode::InternalContent | Mode::StartContent, '\\') => {
                mode = Mode::Escape;
                f(TokenKind::Escape, pos);
            }
            (Mode::StartContent, _) => {
                mode = Mode::InternalContent;
                f(TokenKind::StringContent, pos);
            }
            (Mode::InternalContent, _) => {}
            (Mode::Escape, _) => mode = Mode::StartContent,
        }

        pos += TextSize::from(c.len_utf8() as u32);
    }
}

fn lex_comment_header(offset: TextSize, length: usize, mut f: impl FnMut(TokenKind, TextSize)) {
    f(TokenKind::CommentHeader, offset);

    if length > 4 {
        f(TokenKind::CommentContent, offset + TextSize::from(4));
    }
}

fn lex_comment_definition(
    offset: TextSize,
    length: usize,
    mut f: impl FnMut(TokenKind, TextSize),
) {
    f(TokenKind::CommentDefinition, offset);

    if length > 3 {
        f(TokenKind::CommentContent, offset + TextSize::from(3));
    }
}

fn lex_comment_form(offset: TextSize, length: usize, mut f: impl FnMut(TokenKind, TextSize)) {
    f(TokenKind::CommentForm, offset);

    if length > 2 {
        f(TokenKind::CommentContent, offset + TextSize::from(2));
    }
}

fn lex_comment_line(offset: TextSize, length: usize, mut f: impl FnMut(TokenKind, TextSize)) {
    f(TokenKind::CommentLine, offset);

    if length > 1 {
        f(TokenKind::CommentContent, offset + TextSize::from(1));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let tokens = lex(input);
        expect.assert_debug_eq(&tokens);
    }

    #[test]
    fn lex_nil() {
        check(
            "nil",
            expect![[r#"
                Nil@0..3
            "#]],
        );
    }

    #[test]
    fn lex_boolean_true() {
        check(
            "true",
            expect![[r#"
                Boolean@0..4
            "#]],
        );
    }

    #[test]
    fn lex_boolean_false() {
        check(
            "false",
            expect![[r#"
                Boolean@0..5
            "#]],
        );
    }

    #[test]
    fn lex_integer() {
        check(
            "42",
            expect![[r#"
                Integer@0..2
            "#]],
        );
    }

    #[test]
    fn lex_integer_negative() {
        check(
            "-42",
            expect![[r#"
                Integer@0..3
            "#]],
        );
    }

    #[test]
    fn lex_float() {
        check(
            "42.0",
            expect![[r#"
                Float@0..4
            "#]],
        );
    }

    #[test]
    fn lex_float_negative() {
        check(
            "-42.0",
            expect![[r#"
                Float@0..5
            "#]],
        );
    }

    #[test]
    fn lex_ratio() {
        check(
            "42/42",
            expect![[r#"
                Ratio@0..5
            "#]],
        );
    }

    #[test]
    fn lex_symbol() {
        check(
            "foobar",
            expect![[r#"
                Symbol@0..6
            "#]],
        );
    }

    #[test]
    fn lex_symbol_qualified() {
        check(
            "foobar/baz",
            expect![[r#"
                Symbol@0..10
            "#]],
        );
    }

    #[test]
    fn lex_keyword() {
        check(
            ":foobar",
            expect![[r#"
                Keyword@0..7
            "#]],
        );
    }

    #[test]
    fn lex_keyword_qualified() {
        check(
            ":foobar/baz",
            expect![[r#"
                Keyword@0..11
            "#]],
        );
    }

    #[test]
    fn lex_whitespace() {
        check(
            "  \n , ",
            expect![[r#"
                Whitespace@0..6
            "#]],
        );
    }

    #[test]
    fn lex_hash() {
        check(
            "#",
            expect![[r#"
                Hash@0..1
            "#]],
        );
    }

    #[test]
    fn lex_left_parenthesis() {
        check(
            "(",
            expect![[r#"
                LeftParenthesis@0..1
            "#]],
        );
    }

    #[test]
    fn lex_right_parenthesis() {
        check(
            ")",
            expect![[r#"
                RightParenthesis@0..1
            "#]],
        );
    }

    #[test]
    fn lex_left_bracket() {
        check(
            "[",
            expect![[r#"
                LeftBracket@0..1
            "#]],
        );
    }

    #[test]
    fn lex_right_bracket() {
        check(
            "]",
            expect![[r#"
                RightBracket@0..1
            "#]],
        );
    }

    #[test]
    fn lex_left_brace() {
        check(
            "{",
            expect![[r#"
                LeftBrace@0..1
            "#]],
        );
    }

    #[test]
    fn lex_right_brace() {
        check(
            "}",
            expect![[r#"
                RightBrace@0..1
            "#]],
        );
    }

    #[test]
    fn lex_empty_string() {
        check(
            "\"\"",
            expect![[r#"
                DoubleQuote@0..1
                DoubleQuote@1..2
            "#]],
        );
    }

    #[test]
    fn lex_string() {
        check(
            "\"hello, world!\"",
            expect![[r#"
                DoubleQuote@0..1
                StringContent@1..14
                DoubleQuote@14..15
            "#]],
        );
    }

    #[test]
    fn lex_empty_comment() {
        check(
            ";",
            expect![[r#"
                CommentLine@0..1
            "#]],
        );
    }

    #[test]
    fn lex_comment_line() {
        check(
            "; comment line",
            expect![[r#"
                CommentLine@0..1
                CommentContent@1..14
            "#]],
        );
    }

    #[test]
    fn lex_comment_form() {
        check(
            ";; comment form",
            expect![[r#"
                CommentForm@0..2
                CommentContent@2..15
            "#]],
        );
    }

    #[test]
    fn lex_comment_definition() {
        check(
            ";;; comment definition",
            expect![[r#"
                CommentDefinition@0..3
                CommentContent@3..22
            "#]],
        );
    }

    #[test]
    fn lex_comment_header_or_footer() {
        check(
            ";;;; comment header or footer",
            expect![[r#"
                CommentHeader@0..4
                CommentContent@4..29
            "#]],
        );
    }
}
