use super::*;

fn lex_all(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let tok = lexer.next_token();
        if tok.is_eof() {
            break;
        }
        tokens.push(tok);
    }
    tokens
}

#[test]
fn test_identifiers_and_keywords() {
    let input = "let foo = bar;";
    let tokens = lex_all(input);
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::Keyword,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Identifier,
            TokenKind::SemiColon,
        ]
    );
}

#[test]
fn test_integer_suffixes() {
    let input = "123i32 456u32 789i64 10u64";
    let tokens = lex_all(input);
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::I32Number,
            TokenKind::U32Number,
            TokenKind::I64Number,
            TokenKind::U64Number,
        ]
    );
}

#[test]
fn test_string_literal_with_escape() {
    let input = r#""hello\nworld\"""#;
    let tokens = lex_all(input);
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
    assert_eq!(tokens[0].source, "hello\nworld\"");
}

#[test]
fn test_macros() {
    let input = "@foo @bar(baz)";
    let tokens = lex_all(input);
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![TokenKind::MacroCall, TokenKind::MacroCallWithArgs]
    );
}

#[test]
fn test_macros_nested() {
    let input = "@foo(bar(@baz()))";
    let tokens = lex_all(input);
    let token = &tokens[0];
    assert_eq!(token.kind, TokenKind::MacroCallWithArgs);
    assert_eq!(token.source.as_str(), "foo(bar(@baz()))");
}

#[test]
fn test_comments_and_whitespace() {
    let input = "abc // comment here\n123";
    let tokens = lex_all(input);
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert_eq!(kinds, vec![TokenKind::Identifier, TokenKind::IntegerNumber]);
}

#[test]
fn test_operators() {
    let input = "-> == != && || ...";
    let tokens = lex_all(input);
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::Arrow,
            TokenKind::Eq,
            TokenKind::NotEq,
            TokenKind::DoubleAmpersand,
            TokenKind::DoublePipe,
            TokenKind::Splat,
        ]
    );
}
