use std::mem;

use logos::Logos;
use text_size::TextSize;

use unknown_syntax::TokenKind;
use unknown_token::Tokens;

#[derive(PartialEq, Logos)]
enum LexerTokenKind {
    #[token("nil")]
    Nil,

    #[regex(r#"(true|false)"#)]
    Boolean,

    // TODO: [2023-01-06, Ilshat Sultanov] handle 2r101010, 052, 8r52, 0x2a, 36r16
    #[regex(r#"-?\d+N?"#)]
    Integer,

    // TODO: [2023-01-06, Ilshat Sultanov] handle `M` suffix to support BigDecimals
    #[regex(r#"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+"#)]
    Float,

    #[regex(r#"([-+]?[0-9]+)/([0-9]+)"#)]
    Ratio,

    #[regex(r#"##(-?Inf|NaN)"#)]
    SymbolicValue,

    #[regex(r#"#[a-zA-Z._+]+[/a-zA-Z0-9._+]*"#)]
    TaggedLiteral,

    // TODO: [2023-01-06, Ilshat Sultanov] handle other cases
    #[regex(r#"\\(newline|space|tab|backspace|formfeed|return|.)"#)]
    #[regex(r#"\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]"#)]
    #[regex(r#"\\o([0-7]|([0-7][0-7])|([0-3][0-7][0-7]))"#)]
    Character,

    // TODO: [2023-01-07, Ilshat Sultanov] handle namespace and name with groups
    #[regex(r#"[a-zA-Z._\+\?]+[/a-zA-Z0-9._\+\?\-!]*"#)]
    Symbol,

    #[regex(r#"::?[a-zA-Z._\+\?]+[/a-zA-Z0-9._\+\?\-!]*"#)]
    Keyword,

    // TODO: [2022-12-27, Ilshat Sultanov] handle other whitespace characters
    #[regex(r#"[ ,\n]+"#)]
    Whitespace,

    #[token(":")]
    Colon,

    #[token("^")]
    Caret,

    #[token("*")]
    Asterisk,

    #[token("&")]
    Ampersand,

    #[token("_")]
    Underscore,

    #[token("%")]
    Percent,

    #[token("@")]
    Deref,

    #[token("#")]
    Hash,

    #[token("#_")]
    Discard,

    #[token("#?")]
    ReaderConditional,

    #[token("#?@")]
    ReaderConditionalSplicing,

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

    #[token("'")]
    SingleQuote,

    _DoubleQuote,

    #[token("`")]
    SyntaxQuote,

    #[token("#'")]
    VarQuote,

    #[token("~")]
    Unquote,

    #[token("~@")]
    UnquoteSplicing,

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

        check(
            "foo-bar",
            expect![[r#"
                Symbol@0..7
            "#]],
        );

        check(
            "foobar?",
            expect![[r#"
                Symbol@0..7
            "#]],
        );

        check(
            "foobar!",
            expect![[r#"
                Symbol@0..7
            "#]],
        );

        check(
            "_foobar",
            expect![[r#"
                Symbol@0..7
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

        check(
            "foobar/baz?",
            expect![[r#"
                Symbol@0..11
            "#]],
        );

        check(
            "foobar/baz!",
            expect![[r#"
                Symbol@0..11
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
    fn lex_colon() {
        check(
            ":",
            expect![[r#"
                Colon@0..1
            "#]],
        );
    }

    #[test]
    fn lex_caret() {
        check(
            "^",
            expect![[r#"
                Caret@0..1
            "#]],
        );
    }

    #[test]
    fn lex_asterisk() {
        check(
            "*",
            expect![[r#"
                Asterisk@0..1
            "#]],
        );
    }

    #[test]
    fn lex_ampersand() {
        check(
            "&",
            expect![[r#"
                Ampersand@0..1
            "#]],
        );
    }

    #[test]
    fn lex_underscore() {
        check(
            "_",
            expect![[r#"
                Underscore@0..1
            "#]],
        );
    }

    #[test]
    fn lex_percent() {
        check(
            "%",
            expect![[r#"
                Percent@0..1
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
    fn lex_discard() {
        check(
            "#_",
            expect![[r#"
                Discard@0..2
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
    fn lex_single_quote() {
        check(
            "'",
            expect![[r#"
                SingleQuote@0..1
            "#]],
        );
    }

    #[test]
    fn lex_double_quote() {
        check(
            "\"",
            expect![[r#"
                DoubleQuote@0..1
            "#]],
        );
    }

    #[test]
    fn lex_syntax_quote() {
        check(
            "`",
            expect![[r#"
                SyntaxQuote@0..1
            "#]],
        );
    }

    #[test]
    fn lex_unquote() {
        check(
            "~",
            expect![[r#"
                Unquote@0..1
            "#]],
        );
    }

    #[test]
    fn lex_unquote_splicing() {
        check(
            "~@",
            expect![[r#"
                UnquoteSplicing@0..2
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

    // Clojure syntax

    #[test]
    fn lex_numeric_types() {
        check(
            "42",
            expect![[r#"
                Integer@0..2
            "#]],
        );

        check(
            "-1.5",
            expect![[r#"
                Float@0..4
            "#]],
        );

        check(
            "22/7",
            expect![[r#"
                Ratio@0..4
            "#]],
        );
    }

    #[test]
    fn lex_character_types() {
        check(
            "\\e",
            expect![[r#"
                Character@0..2
            "#]],
        );

        check(
            "\\newline",
            expect![[r#"
                Character@0..8
            "#]],
        );

        check(
            "\\u0041",
            expect![[r#"
                Character@0..6
            "#]],
        );

        check(
            "\\u005A",
            expect![[r#"
                Character@0..6
            "#]],
        );

        check(
            "\\o256",
            expect![[r#"
                Character@0..5
            "#]],
        );

        check(
            "\"\"",
            expect![[r#"
                DoubleQuote@0..1
                DoubleQuote@1..2
            "#]],
        );

        check(
            "\"hello\"",
            expect![[r#"
                DoubleQuote@0..1
                StringContent@1..6
                DoubleQuote@6..7
            "#]],
        );

        check(
            "\"\"",
            expect![[r#"
                DoubleQuote@0..1
                DoubleQuote@1..2
            "#]],
        );

        check(
            "#\"[0-9]+\"",
            expect![[r#"
                Hash@0..1
                DoubleQuote@1..2
                StringContent@2..8
                DoubleQuote@8..9
            "#]],
        );
    }

    #[test]
    fn lex_symbols_and_idents() {
        check(
            "map",
            expect![[r#"
                Symbol@0..3
            "#]],
        );

        check(
            "+",
            expect![[r#"
                Symbol@0..1
            "#]],
        );

        check(
            "clojure.core/+",
            expect![[r#"
                Symbol@0..14
            "#]],
        );

        check(
            "nil",
            expect![[r#"
                Nil@0..3
            "#]],
        );

        check(
            "true false",
            expect![[r#"
                Boolean@0..4
                Whitespace@4..5
                Boolean@5..10
            "#]],
        );

        check(
            ":alpha",
            expect![[r#"
                Keyword@0..6
            "#]],
        );

        check(
            ":release/alpha",
            expect![[r#"
                Keyword@0..14
            "#]],
        );
    }

    #[test]
    fn lex_literal_collections() {
        check(
            "'(1 2 3)",
            expect![[r#"
                SingleQuote@0..1
                LeftParenthesis@1..2
                Integer@2..3
                Whitespace@3..4
                Integer@4..5
                Whitespace@5..6
                Integer@6..7
                RightParenthesis@7..8
            "#]],
        );

        check(
            "'(1, 2, 3)",
            expect![[r#"
                SingleQuote@0..1
                LeftParenthesis@1..2
                Integer@2..3
                Whitespace@3..5
                Integer@5..6
                Whitespace@6..8
                Integer@8..9
                RightParenthesis@9..10
            "#]],
        );

        check(
            "[1 2 3]",
            expect![[r#"
                LeftBracket@0..1
                Integer@1..2
                Whitespace@2..3
                Integer@3..4
                Whitespace@4..5
                Integer@5..6
                RightBracket@6..7
            "#]],
        );

        check(
            "[1, 2, 3]",
            expect![[r#"
                LeftBracket@0..1
                Integer@1..2
                Whitespace@2..4
                Integer@4..5
                Whitespace@5..7
                Integer@7..8
                RightBracket@8..9
            "#]],
        );

        check(
            "#{1 2 3}",
            expect![[r#"
                Hash@0..1
                LeftBrace@1..2
                Integer@2..3
                Whitespace@3..4
                Integer@4..5
                Whitespace@5..6
                Integer@6..7
                RightBrace@7..8
            "#]],
        );

        check(
            "#{1, 2, 3}",
            expect![[r#"
                Hash@0..1
                LeftBrace@1..2
                Integer@2..3
                Whitespace@3..5
                Integer@5..6
                Whitespace@6..8
                Integer@8..9
                RightBrace@9..10
            "#]],
        );

        check(
            "{:a 1 :b 2]",
            expect![[r#"
                LeftBrace@0..1
                Keyword@1..3
                Whitespace@3..4
                Integer@4..5
                Whitespace@5..6
                Keyword@6..8
                Whitespace@8..9
                Integer@9..10
                RightBracket@10..11
            "#]],
        );

        check(
            "{:a 1, :b 2}",
            expect![[r#"
                LeftBrace@0..1
                Keyword@1..3
                Whitespace@3..4
                Integer@4..5
                Whitespace@5..7
                Keyword@7..9
                Whitespace@9..10
                Integer@10..11
                RightBrace@11..12
            "#]],
        );
    }

    #[test]
    fn lex_empty_list() {
        check(
            "'()",
            expect![[r#"
                SingleQuote@0..1
                LeftParenthesis@1..2
                RightParenthesis@2..3
            "#]],
        );
    }

    #[test]
    fn lex_empty_vector() {
        check(
            "[]",
            expect![[r#"
                LeftBracket@0..1
                RightBracket@1..2
            "#]],
        );
    }

    #[test]
    fn lex_empty_map() {
        check(
            "{}",
            expect![[r#"
                LeftBrace@0..1
                RightBrace@1..2
            "#]],
        );
    }

    #[test]
    fn lex_dispatch_character() {
        check(
            "#",
            expect![[r#"
                Hash@0..1
            "#]],
        );
    }

    #[test]
    fn lex_empty_set() {
        check(
            "#{}",
            expect![[r#"
                Hash@0..1
                LeftBrace@1..2
                RightBrace@2..3
            "#]],
        );
    }

    #[test]
    fn lex_ignore_the_next_form() {
        check(
            "#_",
            expect![[r#"
                Discard@0..2
            "#]],
        );
    }

    #[test]
    fn lex_empty_regex() {
        check(
            "#\"\"",
            expect![[r#"
                Hash@0..1
                DoubleQuote@1..2
                DoubleQuote@2..3
            "#]],
        );
    }

    #[test]
    fn lex_anonymous_function() {
        check(
            "#()",
            expect![[r#"
                Hash@0..1
                LeftParenthesis@1..2
                RightParenthesis@2..3
            "#]],
        );
    }

    #[test]
    fn lex_var_quote() {
        check(
            "#'",
            expect![[r#"
                VarQuote@0..2
            "#]],
        );
    }

    #[test]
    fn lex_symbolic_values() {
        check(
            "##Inf",
            expect![[r#"
                SymbolicValue@0..5
            "#]],
        );

        check(
            "##-Inf",
            expect![[r#"
                SymbolicValue@0..6
            "#]],
        );

        check(
            "##NaN",
            expect![[r#"
                SymbolicValue@0..5
            "#]],
        );
    }

    #[test]
    fn lex_tagged_literals() {
        check(
            "#uuid",
            expect![[r#"
                TaggedLiteral@0..5
            "#]],
        );

        check(
            "#inst",
            expect![[r#"
                TaggedLiteral@0..5
            "#]],
        );
    }

    #[test]
    fn lex_anonymous_function_arguments() {
        check(
            "%",
            expect![[r#"
                Percent@0..1
            "#]],
        );

        check(
            "%1",
            expect![[r#"
                Percent@0..1
                Integer@1..2
            "#]],
        );

        check(
            "%2",
            expect![[r#"
                Percent@0..1
                Integer@1..2
            "#]],
        );
    }

    #[test]
    fn lex_deref() {
        check(
            "@",
            expect![[r#"
                Deref@0..1
            "#]],
        );
    }

    #[test]
    fn lex_metadata() {
        check(
            "^{}",
            expect![[r#"
                Caret@0..1
                LeftBrace@1..2
                RightBrace@2..3
            "#]],
        );

        check(
            "^:private",
            expect![[r#"
                Caret@0..1
                Keyword@1..9
            "#]],
        );

        check(
            "^{:private true}",
            expect![[r#"
                Caret@0..1
                LeftBrace@1..2
                Keyword@2..10
                Whitespace@10..11
                Boolean@11..15
                RightBrace@15..16
            "#]],
        );
    }

    #[test]
    fn lex_quote() {
        check(
            "'",
            expect![[r#"
                SingleQuote@0..1
            "#]],
        );
    }

    #[test]
    fn lex_comment() {
        check(
            "; ignore me",
            expect![[r#"
                CommentLine@0..1
                CommentContent@1..11
            "#]],
        );
    }

    #[test]
    fn lex_keyword() {
        check(
            ":keyword",
            expect![[r#"
                Keyword@0..8
            "#]],
        );

        check(
            "::keyword",
            expect![[r#"
                Keyword@0..9
            "#]],
        );
    }

    #[test]
    fn lex_map_namespaced() {
        check(
            "#:person{:age 42}",
            expect![[r#"
                Hash@0..1
                Keyword@1..8
                LeftBrace@8..9
                Keyword@9..13
                Whitespace@13..14
                Integer@14..16
                RightBrace@16..17
            "#]],
        );
    }

    #[test]
    fn lex_symbol_gensym() {
        check(
            "foobar#",
            expect![[r#"
                Symbol@0..6
                Hash@6..7
            "#]],
        );
    }

    #[test]
    fn lex_reader_conditional() {
        check(
            "#?(:clj 42)",
            expect![[r#"
                ReaderConditional@0..2
                LeftParenthesis@2..3
                Keyword@3..7
                Whitespace@7..8
                Integer@8..10
                RightParenthesis@10..11
            "#]],
        );

        check(
            "#?@(:clj [42])",
            expect![[r#"
                ReaderConditionalSplicing@0..3
                LeftParenthesis@3..4
                Keyword@4..8
                Whitespace@8..9
                LeftBracket@9..10
                Integer@10..12
                RightBracket@12..13
                RightParenthesis@13..14
            "#]],
        );
    }

    #[test]
    fn lex_earmuffs() {
        check(
            "*foo*",
            expect![[r#"
                Asterisk@0..1
                Symbol@1..4
                Asterisk@4..5
            "#]],
        );
    }
}
