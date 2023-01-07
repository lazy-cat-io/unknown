use std::mem;

pub type SyntaxBuilder = eventree::SyntaxBuilder<TreeConfig>;
pub type SyntaxElement = eventree::SyntaxElement<TreeConfig>;
pub type SyntaxNode = eventree::SyntaxNode<TreeConfig>;
pub type SyntaxToken = eventree::SyntaxToken<TreeConfig>;
pub type SyntaxTree = eventree::SyntaxTree<TreeConfig>;
pub type Event = eventree::Event<TreeConfig>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TreeConfig {}

impl eventree::TreeConfig for TreeConfig {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Nil,
    Boolean,
    Integer,
    Float,
    Ratio,
    SymbolicValue,
    TaggedLiteral,
    Character,
    Symbol,
    Keyword,

    Whitespace,
    Colon,
    Caret,
    Asterisk,
    Ampersand,
    Underscore,
    Percent,
    Deref,
    Hash,
    Discard,
    ReaderConditional,
    ReaderConditionalSplicing,
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    SingleQuote,
    DoubleQuote,
    SyntaxQuote,
    VarQuote,
    Unquote,
    UnquoteSplicing,
    Escape,
    StringContent,
    CommentContent,
    CommentHeader,
    CommentDefinition,
    CommentForm,
    CommentLine,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
    Root,
    Nil,
    Boolean,
    Integer,
    Float,
    Ratio,
    Character,
    String,
    Symbol,
    Keyword,

    List,
    Vector,
    Map,
    Set,

    Comment,
    Error,
}

unsafe impl eventree::SyntaxKind for TokenKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw as u8)
    }
}

unsafe impl eventree::SyntaxKind for NodeKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw as u8)
    }
}
