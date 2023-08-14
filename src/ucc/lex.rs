use bitflags::bitflags;
use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    ops,
    str::Chars,
};
use tracing::debug;

use super::error::{UccError, UccResult};

#[derive(Debug, PartialEq, Clone)]
#[repr(i16)]
pub enum TokenType {
    // keywords
    KwdVoid = 0,
    KwdSigned,
    KwdUnsigned,
    KwdBool,
    KwdChar,
    KwdShort,
    KwdInt,
    KwdLong,
    KwdFloat,
    KwdDouble,
    KwdDo,
    KwdWhile,
    KwdFor,
    KwdIf,
    KwdElse,
    KwdBreak,
    KwdContinue,
    KwdReturn,
    KwdSwitch,
    KwdCase,
    KwdGoto,
    KwdAuto,
    KwdExtern,
    KwdRegister,
    KwdRestrict,
    KwdStatic,
    KwdTypedef,
    KwdUnion,
    KwdStruct,
    KwdEnum,
    KwdConst,
    KwdSizeof,
    KwdVolatile,
    // id
    Identifier(String),
    // values
    Int(u64),
    Float(f64),
    Char(char),
    String(String),
    // assign
    OpAssign,           // =
    OpMulAssign,        // *=
    OpDivAssign,        // /=
    OpModAssign,        // %=
    OpAddAssign,        // +=
    OpSubAssign,        // -=
    OpLeftShiftAssign,  // <<=
    OpRightShiftAssign, // >>=
    OpAndAssign,        // &=
    OpXorAssign,        // ^=
    OpOrAssign,         // |=
    // punctuators
    PuncLeftBrace,        // {
    PuncRightBrace,       // }
    PuncLeftParen,        // (
    PuncRightParen,       // )
    PuncLeftBracket,      // [
    PuncRightBracket,     // ]
    PuncDot,              // .
    PuncArrow,            // ->
    PuncPlusPlus,         // ++
    PuncMinusMinus,       // --
    PuncAmpersand,        // &
    PuncAsterisk,         // *
    PuncPlus,             // +
    PuncMinus,            // -
    PuncTilde,            // ~
    PuncExclamation,      // !
    PuncSlash,            // /
    PuncPercent,          // %
    PuncLeftShift,        // <<
    PuncRightShift,       // >>
    PuncLessThan,         // <
    PuncGreaterThan,      // >
    PuncLessThanEqual,    // <=
    PuncGreaterThanEqual, // >=
    PuncEqual,            // ==
    PuncNotEqual,         // !=
    PuncCaret,            // ^
    PuncPipe,             // |
    PuncLAnd,             // &&
    PuncLOr,              // ||
    PuncQuestion,         // ?
    PuncColon,            // :
    PuncSemicolon,        // ;
    PuncComma,            // ,
    // comment
    Comment,
    // eof
    EOF = 255,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

impl Pos {
    #[inline]
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl ops::Sub<usize> for Pos {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        if self.column >= rhs {
            Self {
                line: self.line,
                column: self.column - rhs,
            }
        } else {
            Self {
                line: self.line,
                column: 0,
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub start: Pos,
    pub end: Pos,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = [
        ("void", TokenType::KwdVoid),
        ("signed", TokenType::KwdSigned),
        ("unsigned", TokenType::KwdUnsigned),
        ("_Bool", TokenType::KwdBool),
        ("char", TokenType::KwdChar),
        ("short", TokenType::KwdShort),
        ("int", TokenType::KwdInt),
        ("long", TokenType::KwdLong),
        ("float", TokenType::KwdFloat),
        ("double", TokenType::KwdDouble),
        ("do", TokenType::KwdDo),
        ("while", TokenType::KwdWhile),
        ("for", TokenType::KwdFor),
        ("if", TokenType::KwdIf),
        ("else", TokenType::KwdElse),
        ("break", TokenType::KwdBreak),
        ("continue", TokenType::KwdContinue),
        ("return", TokenType::KwdReturn),
        ("switch", TokenType::KwdSwitch),
        ("case", TokenType::KwdCase),
        ("goto", TokenType::KwdGoto),
        ("auto", TokenType::KwdAuto),
        ("extern", TokenType::KwdExtern),
        ("register", TokenType::KwdRegister),
        ("restrict", TokenType::KwdRestrict),
        ("static", TokenType::KwdStatic),
        ("typedef", TokenType::KwdTypedef),
        ("union", TokenType::KwdUnion),
        ("struct", TokenType::KwdStruct),
        ("enum", TokenType::KwdEnum),
        ("const", TokenType::KwdConst),
        ("sizeof", TokenType::KwdSizeof),
        ("volatile", TokenType::KwdVolatile),
    ].into();
    static ref OPERATORS: HashMap<&'static str, TokenType> = [
        ("{", TokenType::PuncLeftBrace),
        ("}", TokenType::PuncRightBrace),
        ("(", TokenType::PuncLeftParen),
        (")", TokenType::PuncRightParen),
        ("[", TokenType::PuncLeftBracket),
        ("]", TokenType::PuncRightBracket),
        (".", TokenType::PuncDot),
        ("->", TokenType::PuncArrow),
        ("++", TokenType::PuncPlusPlus),
        ("--", TokenType::PuncMinusMinus),
        ("&", TokenType::PuncAmpersand),
        ("*", TokenType::PuncAsterisk),
        ("+", TokenType::PuncPlus),
        ("-", TokenType::PuncMinus),
        ("~", TokenType::PuncTilde),
        ("!", TokenType::PuncExclamation),
        ("/", TokenType::PuncSlash),
        ("%", TokenType::PuncPercent),
        ("<<", TokenType::PuncLeftShift),
        (">>", TokenType::PuncRightShift),
        ("<", TokenType::PuncLessThan),
        (">", TokenType::PuncGreaterThan),
        ("<=", TokenType::PuncLessThanEqual),
        (">=", TokenType::PuncGreaterThanEqual),
        ("==", TokenType::PuncEqual),
        ("!=", TokenType::PuncNotEqual),
        ("^", TokenType::PuncCaret),
        ("|", TokenType::PuncPipe),
        ("&&", TokenType::PuncLAnd),
        ("||", TokenType::PuncLOr),
        ("?", TokenType::PuncQuestion),
        (":", TokenType::PuncColon),
        (";", TokenType::PuncSemicolon),
        (",", TokenType::PuncComma),
        // assignment
        ("=", TokenType::OpAssign),
        ("*=", TokenType::OpMulAssign),
        ("/=", TokenType::OpDivAssign),
        ("%=", TokenType::OpModAssign),
        ("+=", TokenType::OpAddAssign),
        ("-=", TokenType::OpSubAssign),
        ("<<=", TokenType::OpLeftShiftAssign),
        (">>=", TokenType::OpRightShiftAssign),
        ("&=", TokenType::OpAndAssign),
        ("^=", TokenType::OpXorAssign),
        ("|=", TokenType::OpOrAssign),
    ].into();
    static ref SPECIAL_CHARS: HashSet<char> = [
        '{', '}', '(', ')', '[', ']', '.', '-', '+', '~', '!', '/', '%', '<', '>', '^', '|', '&',
        '*', '?', ':', ';', '=', ',', '#',
    ].into();
}

bitflags! {
    struct SuffixFlags: u8 {
        const SIGNED = 0b00000001;
        const UNSIGNED = 0b00000010;
        const LONG = 0b00000100;
        const LONGLONG = 0b00001000;
        const FLOAT = 0b00010000;
        const DOUBLE = 0b00100000;
    }
}

pub struct Lexer<'a> {
    input: Chars<'a>,
    cur_char: Option<char>,
    pos: usize,
    cursor_pos: Pos,
    buf_start_pos: Pos,
    buffer: String,
}

#[inline]
fn is_stop_char(c: char) -> bool {
    c.is_ascii_whitespace() || SPECIAL_CHARS.contains(&c)
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input.chars(),
            cur_char: None,
            pos: 0,
            cursor_pos: Pos::new(1, 0),
            buf_start_pos: Pos::new(1, 0),
            buffer: String::new(),
        }
    }

    #[inline]
    fn error<T>(&self, message: String) -> UccResult<T> {
        Err(UccError {
            message,
            line: self.cursor_pos.line,
            column: self.cursor_pos.column,
        })
    }

    #[inline]
    fn new_token(&self, ty: TokenType) -> Token {
        Token {
            ty,
            start: self.buf_start_pos,
            end: self.cursor_pos,
        }
    }

    #[inline]
    fn new_token_with_start_pos(&self, ty: TokenType, start: Pos) -> Token {
        Token {
            ty,
            start,
            end: self.cursor_pos - 1,
        }
    }

    #[inline]
    fn new_token_buf_pos(&self, ty: TokenType) -> Token {
        Token {
            ty,
            start: self.buf_start_pos,
            end: self.cursor_pos,
        }
    }

    fn peek1(&mut self) -> Option<char> {
        self.input.clone().next()
    }

    fn peek2(&mut self) -> Option<(char, char)> {
        let mut iter = self.input.clone();
        let c1 = iter.next();
        if c1.is_none() {
            return None;
        }
        let c2 = iter.next();
        if let Some(c2) = c2 {
            Some((c1.unwrap(), c2))
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.input.next();
        if let Some(c) = c {
            self.pos += 1;
            if c == '\n' {
                self.cursor_pos.line += 1;
                self.cursor_pos.column = 0;
            } else {
                self.cursor_pos.column += 1;
            }
        }
        self.cur_char = c;
        c
    }

    fn skip_whitesp(&mut self) {
        while let Some(c) = self.cur_char {
            if !c.is_ascii_whitespace() {
                break;
            }
            self.next();
        }
    }

    #[inline]
    fn clear_buffer(&mut self) {
        self.buffer.clear();
        self.buf_start_pos = self.cursor_pos;
    }

    #[inline]
    fn read_unicode_escape(&mut self) -> UccResult<()> {
        let mut n = 0;
        let mut val = 0;
        while n < 4 {
            let c = self.next().unwrap();
            if c.is_ascii_hexdigit() {
                val = val * 16 + c.to_digit(16).unwrap();
                n += 1;
            } else {
                return self.error(format!("invalid unicode escape: {}", c));
            }
        }
        self.buffer.push(char::from_u32(val).unwrap());
        Ok(())
    }

    fn read_escaped_char(&mut self) -> UccResult<()> {
        let c = self.next().unwrap();
        match c {
            'n' => self.buffer.push('\n'),
            't' => self.buffer.push('\t'),
            'r' => self.buffer.push('\r'),
            'u' => return self.read_unicode_escape(),
            '\\' => self.buffer.push('\\'),
            '0' => self.buffer.push('\0'),
            _ => {
                self.buffer.push('\\');
                self.buffer.push(c)
            }
        }
        Ok(())
    }

    fn read_string(&mut self) -> UccResult<Token> {
        self.clear_buffer();
        while let Some(c) = self.next() {
            let r = match c {
                '"' => break,
                '\r' | '\n' => return self.error(format!("missing terminating \" character")),
                '\\' => self.read_escaped_char(),
                _ => {
                    self.buffer.push(c);
                    Ok(())
                }
            };
            if let Err(e) = r {
                return Err(e);
            }
        }
        if self.cur_char.is_none() {
            return self.error(format!("unexpected eof"));
        }
        self.next(); // consume "
        debug!("string: {}", self.buffer);
        Ok(self.new_token_buf_pos(TokenType::String(self.buffer.clone())))
    }

    fn read_char(&mut self) -> UccResult<Token> {
        self.clear_buffer();
        if let Some(c) = self.next() {
            let r = match c {
                '\'' => return self.error(format!("empty char")),
                '\r' | '\n' => return self.error(format!("missing terminating ' character")),
                '\\' => self.read_escaped_char(),
                _ => {
                    self.buffer.push(c);
                    Ok(())
                }
            };
            if let Err(e) = r {
                return Err(e);
            }
            if let Some(c) = self.next() {
                if c != '\'' {
                    return self.error(format!("invalid char: {}", self.buffer));
                }
                self.next(); // consume '
            } else {
                return self.error(format!("unexpected eof"));
            }
        } else {
            return self.error(format!("unexpected eof"));
        }
        debug!("char: {}", self.buffer);
        Ok(self.new_token_buf_pos(TokenType::Char(self.buffer.chars().next().unwrap())))
    }

    fn read_int_suffix(&mut self) -> UccResult<SuffixFlags> {
        let mut suffix = SuffixFlags::empty();
        let c = if let Some(c) = self.cur_char {
            c
        } else {
            return Ok(suffix);
        };
        if c == 'u' || c == 'U' {
            suffix |= SuffixFlags::UNSIGNED;
            self.next();
        }
        if let Some(c) = self.cur_char {
            if c == 'l' || c == 'L' {
                suffix |= SuffixFlags::LONG;
                self.next();
            }
        } else {
            return Ok(suffix);
        }
        if let Some(c) = self.cur_char {
            if c == 'l' || c == 'L' {
                suffix |= SuffixFlags::LONGLONG;
                self.next();
            }
        } else {
            return Ok(suffix);
        }
        if let Some(c) = self.cur_char {
            if (c == 'u' || c == 'U') && !suffix.contains(SuffixFlags::UNSIGNED) {
                suffix |= SuffixFlags::UNSIGNED;
                self.next();
            }
        }
        Ok(suffix)
    }

    fn read_float_suffix(&mut self) -> UccResult<SuffixFlags> {
        let mut suffix = SuffixFlags::empty();
        let c = if let Some(c) = self.cur_char {
            c
        } else {
            return Ok(suffix);
        };
        if c == 'f' || c == 'F' {
            suffix |= SuffixFlags::FLOAT;
            self.next();
        } else if c == 'd' || c == 'D' {
            suffix |= SuffixFlags::DOUBLE;
            self.next();
        }
        Ok(suffix)
    }

    fn read_float(&mut self) -> UccResult<Token> {
        if let Ok(_suffix) = self.read_float_suffix() {
            // noop
        } else {
            return self.error(format!("invalid float suffix"));
        }
        let val = self.buffer.parse::<f64>().unwrap();
        debug!("float: {}", self.buffer);
        Ok(self.new_token_buf_pos(TokenType::Float(val)))
    }

    fn read_int(&mut self, radix: u32) -> UccResult<Token> {
        if let Ok(_suffix) = self.read_int_suffix() {
            // noop
        } else {
            return self.error(format!("invalid int suffix"));
        };
        let val = u64::from_str_radix(self.buffer.as_str(), radix).unwrap();
        debug!("int: {}", self.buffer);
        Ok(self.new_token_buf_pos(TokenType::Int(val)))
    }

    fn read_num(&mut self) -> UccResult<Token> {
        self.clear_buffer();
        let c = self.cur_char.unwrap();
        let mut radix: u32 = 10;
        if c == '0' {
            if let Some(c1) = self.peek1() {
                match c1 {
                    'x' | 'X' | 'b' | 'B' => {
                        self.next(); // consume 0
                        self.next(); // consume x/X/b/B
                        match c1 {
                            'x' | 'X' => {
                                radix = 16;
                            }
                            'b' | 'B' => {
                                radix = 2;
                            }
                            _ => unreachable!(),
                        }
                    }
                    '0'..='9' => {
                        self.next(); // consume 0
                        radix = 8;
                    }
                    _ => {}
                }
            }
        }
        let mut has_dot = false;
        while let Some(c) = self.cur_char {
            match c {
                '2'..='9' if radix == 2 => {
                    return self.error(format!("invalid character {} on binary constant", c));
                }
                '8' | '9' if radix == 8 => {
                    return self.error(format!("invalid character {} on octal constant", c));
                }
                'a'..='f' | 'A'..='F' if radix == 16 => {
                    self.buffer.push(c);
                    self.next();
                    continue;
                }
                '.' => {
                    if has_dot || (radix != 10 && radix != 8) {
                        return self.error(format!("invalid character: {}", c));
                    }
                    has_dot = true;
                }
                '0'..='9' => {}
                _ => {
                    break;
                }
            }
            self.buffer.push(c);
            self.next();
        }
        if has_dot {
            return self.read_float();
        }
        return self.read_int(radix);
    }

    fn read_comments(&mut self) -> UccResult<Token> {
        let start_pos = self.cursor_pos;
        let c = self.cur_char.unwrap(); // checked due to peek
        if c == '/' {
            while let Some(c) = self.next() {
                if c == '\n' {
                    return Ok(self.new_token_with_start_pos(TokenType::Comment, start_pos));
                }
            }
            return Ok(self.new_token(TokenType::EOF));
        }
        // if c == '*'
        while let Some(c) = self.next() {
            if c == '*' {
                if let Some(c) = self.next() {
                    if c == '/' {
                        return Ok(self.new_token_with_start_pos(TokenType::Comment, start_pos));
                    }
                }
            }
        }
        return self.error(format!("unterminated comment"));
    }

    fn read_special_char(&mut self) -> UccResult<Token> {
        let start_pos = self.cursor_pos;
        let c = self.cur_char.unwrap();
        if c == '/' {
            if let Some(c1) = self.peek1() {
                return match c1 {
                    '/' | '*' => {
                        self.next(); // consume /
                        self.next(); // consume / or *
                        self.read_comments()
                    }
                    '=' => {
                        self.next(); // consume /
                        self.next(); // consume =
                        Ok(self.new_token_with_start_pos(TokenType::OpDivAssign, start_pos))
                    }
                    _ => Ok(self.new_token_with_start_pos(TokenType::PuncSlash, start_pos)),
                };
            }
            return Ok(self.new_token_with_start_pos(TokenType::PuncSlash, start_pos));
        }
        if let Some(c1) = self.peek1() {
            if SPECIAL_CHARS.contains(&c1) {
                if let Some((_, c2)) = self.peek2() {
                    if SPECIAL_CHARS.contains(&c2) {
                        let s = format!("{}{}{}", c, c1, c2);
                        if let Some(token) = OPERATORS.get(&s.as_str()) {
                            self.next();
                            self.next();
                            self.next();
                            return Ok(self.new_token_with_start_pos(token.clone(), start_pos));
                        }
                    }
                }
                let s = format!("{}{}", c, c1);
                if let Some(token) = OPERATORS.get(&s.as_str()) {
                    self.next();
                    self.next();
                    return Ok(self.new_token_with_start_pos(token.clone(), start_pos));
                }
            }
        }
        self.next(); // consume c
        return Ok(self.new_token_with_start_pos(
            OPERATORS.get(&c.to_string().as_str()).unwrap().clone(),
            start_pos,
        ));
    }

    fn read_keyword_or_ident(&mut self) -> UccResult<Token> {
        self.clear_buffer();
        let start_pos = self.cursor_pos;
        while let Some(c) = self.cur_char {
            if !is_stop_char(c) {
                self.buffer.push(c);
                self.next(); // consume c
            } else {
                break;
            }
        }
        assert!(self.buffer.len() > 0);
        if let Some(token) = KEYWORDS.get(&self.buffer.as_str()) {
            debug!("keyword: {}", self.buffer);
            return Ok(self.new_token_with_start_pos(token.clone(), start_pos));
        }
        debug!("ident: {}", self.buffer);
        Ok(self.new_token_with_start_pos(TokenType::Identifier(self.buffer.clone()), start_pos))
    }

    fn next_token(&mut self) -> UccResult<Token> {
        self.skip_whitesp();
        if let None = self.cur_char {
            return Ok(self.new_token(TokenType::EOF));
        }
        let c = self.cur_char.unwrap();
        return match c {
            '"' => self.read_string(),
            '\'' => self.read_char(),
            '0'..='9' | '.' => self.read_num(),
            _ if SPECIAL_CHARS.contains(&c) => self.read_special_char(),
            _ => self.read_keyword_or_ident(),
        };
    }

    pub fn lex(&mut self) -> UccResult<Vec<Token>> {
        let mut tokens = Vec::new();
        self.next();
        loop {
            let result = self.next_token();
            if let Err(e) = result {
                return Err(e);
            }
            let token = result.unwrap();
            let is_eof = token.ty == TokenType::EOF;
            debug!("token: {:?}", token);
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }
}
