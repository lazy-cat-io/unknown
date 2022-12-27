use std::mem;

use logos::Logos;
use text_size::TextSize;

use unknown_syntax::TokenKind;
use unknown_token::Tokens;

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
            LexerTokenKind::__InternalComment => lex_comment(start, range.len(), handler),
            _ => handler(unsafe { mem::transmute(kind) }, start),
        }
    }

    starts.push((text.len() as u32).into());

    kinds.shrink_to_fit();
    starts.shrink_to_fit();

    Tokens::new(kinds, starts)
}

#[derive(PartialEq, Logos)]
enum LexerTokenKind {
    // TODO: [2022-12-27, Ilshat Sultanov] handle other whitespace characters
    #[regex("[ ,\n]+")]
    Whitespace,

    #[token(",")]
    Comma,

    #[token("nil")]
    Nil,

    #[regex("true|false")]
    Boolean,

    _CommentLeader,
    _CommentContent,

    #[regex(";.*")]
    __InternalComment,

    #[error]
    Error,
}

fn lex_comment(offset: TextSize, len: usize, mut f: impl FnMut(TokenKind, TextSize)) {
    f(TokenKind::CommentLeader, offset);

    if len > 1 {
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
    fn lex_whitespace() {
        check(
            "  \n , ",
            expect![[r#"
                Whitespace@0..6
            "#]],
        );
    }

    #[test]
    fn lex_comma() {
        check(
            ",",
            expect![[r#"
                Comma@0..1
            "#]],
        );
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
    fn lex_empty_comment() {
        check(
            ";",
            expect![[r#"
                CommentLeader@0..1
            "#]],
        );
    }

    #[test]
    fn lex_comment_column() {
        check(
            "; comment column",
            expect![[r#"
                CommentLeader@0..1
                CommentContent@1..16
            "#]],
        );
    }

    #[test]
    fn lex_comment_form() {
        check(
            ";; comment form",
            expect![[r#"
                CommentLeader@0..1
                CommentContent@1..15
            "#]],
        );
    }

    #[test]
    fn lex_comment_definition() {
        check(
            ";;; comment definition",
            expect![[r#"
                CommentLeader@0..1
                CommentContent@1..22
            "#]],
        );
    }

    #[test]
    fn lex_comment_header_or_footer() {
        check(
            ";;;; comment header/footer",
            expect![[r#"
                CommentLeader@0..1
                CommentContent@1..26
            "#]],
        );
    }
}
