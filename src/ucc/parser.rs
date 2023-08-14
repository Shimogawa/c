use std::borrow::BorrowMut;

use tracing::debug;

use super::{
    ast::{Decl, FuncType, NumberType, Program, Stmt, Type, TypeKind, VarDecl},
    error::{UccError, UccResult},
    lex::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    #[inline]
    fn cur_token(&self) -> Option<&Token> {
        self.tokens.get(self.idx)
    }

    fn next_token(&mut self) -> Option<&Token> {
        self.idx += 1;
        self.cur_token()
    }

    fn error<T>(&self, msg: &str) -> UccResult<T> {
        let token = match self.cur_token() {
            Some(token) => token,
            None => {
                return Err(UccError {
                    message: msg.to_string(),
                    line: 0,
                    column: 0,
                })
            }
        };
        Err(UccError {
            message: msg.to_string(),
            line: token.start.line,
            column: token.start.column,
        })
    }

    fn expect(&mut self, token_type: TokenType) -> UccResult<()> {
        if let Some(token) = self.cur_token() {
            if token.ty == token_type {
                self.next_token();
                return Ok(());
            } else {
                return self.error("unexpected token");
            }
        } else {
            return self.error("no token to expect");
        }
    }

    fn is_type_specifier(&mut self) -> bool {
        if let Some(token) = self.cur_token() {
            match token.ty {
                TokenType::KwdVoid
                | TokenType::KwdBool
                | TokenType::KwdChar
                | TokenType::KwdShort
                | TokenType::KwdInt
                | TokenType::KwdLong
                | TokenType::KwdFloat
                | TokenType::KwdDouble
                | TokenType::KwdSigned
                | TokenType::KwdUnsigned => true,
                _ => false,
            }
        } else {
            false
        }
    }

    fn is_type_qualifier(&mut self) -> bool {
        if let Some(token) = self.cur_token() {
            match token.ty {
                // TokenType::KwdRestrict
                // | TokenType::KwdRegister
                TokenType::KwdConst | TokenType::KwdVolatile => true,
                _ => false,
            }
        } else {
            false
        }
    }

    fn fill_in_inner_type(&self, ty: &mut Type, inner_ty: Type) -> UccResult<()> {
        let mut ptr = ty;
        loop {
            match ptr.ty {
                TypeKind::Ptr(ref mut t) => match t.as_mut().ty {
                    TypeKind::Invalid => {
                        *t.as_mut() = inner_ty;
                        break;
                    }
                    _ => ptr = t.as_mut(),
                },
                TypeKind::Array(ref mut t, _) => match t.as_mut().ty {
                    TypeKind::Invalid => {
                        *t.as_mut() = inner_ty;
                        break;
                    }
                    _ => ptr = t.as_mut(),
                },
                TypeKind::Func(ref mut t) => match t.ret.as_mut().ty {
                    TypeKind::Invalid => {
                        *t.ret.as_mut() = inner_ty;
                        break;
                    }
                    _ => ptr = t.ret.as_mut(),
                },
                _ => return self.error("invalid type to fill"),
            }
        }
        Ok(())
    }

    // fn parse_stmt(&mut self) -> UccResult<Stmt> {}

    // declaration-specifiers :: type-qualifier* type-specifier*
    fn parse_declaration_specifier(&mut self) -> UccResult<Type> {
        let mut ty = Type::new(TypeKind::Void);
        let mut token_type = None;
        let mut is_signed = None;
        loop {
            if self.is_type_specifier() {
                let token = self.cur_token().unwrap();
                match token.ty {
                    TokenType::KwdUnsigned | TokenType::KwdSigned => {
                        if !is_signed.is_none() {
                            return self.error("cannot have duplicates in signed or unsigned");
                        }
                        is_signed = Some(token.ty == TokenType::KwdSigned);
                        token_type = Some(TokenType::KwdInt);
                    }
                    TokenType::KwdChar => {
                        if token_type.is_none() {
                            token_type = Some(TokenType::KwdChar);
                        } else {
                            match token_type {
                                Some(
                                    TokenType::KwdShort | TokenType::KwdInt | TokenType::KwdLong,
                                ) => return self.error("cannot have 2 or more type specifiers"),
                                Some(TokenType::KwdChar) => (),
                                None => token_type = Some(TokenType::KwdChar),
                                Some(_) => {
                                    return self.error("cannot have 2 or more type specifiers")
                                }
                            }
                        }
                    }
                    TokenType::KwdInt => match token_type {
                        Some(TokenType::KwdChar | TokenType::KwdShort) | None => {
                            token_type = Some(TokenType::KwdInt)
                        }
                        Some(TokenType::KwdLong | TokenType::KwdInt) => (),
                        Some(_) => return self.error("cannot have 2 or more type specifiers"),
                    },
                    TokenType::KwdLong => match token_type {
                        Some(TokenType::KwdInt) => token_type = Some(TokenType::KwdLong),
                        Some(TokenType::KwdLong) => token_type = Some(TokenType::KwdLong),
                        Some(_) => return self.error("cannot have 2 or more type specifiers"),
                        None => token_type = Some(TokenType::KwdLong),
                    },
                    TokenType::KwdFloat | TokenType::KwdDouble => {
                        if token_type.is_none() {
                            token_type = Some(token.ty.clone());
                        } else {
                            return self.error("cannot have 2 or more type specifiers");
                        }
                    }
                    _ => {
                        return self
                            .error(format!("invalid type specifier '{:?}'", token.ty).as_str())
                    }
                };
                self.next_token();
            } else if self.is_type_qualifier() {
                let token = self.cur_token().unwrap();
                match token.ty {
                    TokenType::KwdConst => ty.is_const = true,
                    TokenType::KwdVolatile => ty.is_volatile = true,
                    _ => {
                        return self
                            .error(format!("invalid type qualifier '{:?}'", token.ty).as_str())
                    }
                };
                self.next_token();
            } else {
                break;
            }
        }
        if token_type.is_none() {
            return self.error("expected type specifier");
        }
        if is_signed.is_none() {
            is_signed = Some(true);
        }
        ty.ty = match token_type.unwrap() {
            TokenType::KwdBool => TypeKind::Bool,
            TokenType::KwdChar => TypeKind::Char(NumberType {
                signed: is_signed.unwrap(),
            }),
            TokenType::KwdShort => TypeKind::Short(NumberType {
                signed: is_signed.unwrap(),
            }),
            TokenType::KwdInt => TypeKind::Int(NumberType {
                signed: is_signed.unwrap(),
            }),
            TokenType::KwdLong => TypeKind::Long(NumberType {
                signed: is_signed.unwrap(),
            }),
            TokenType::KwdFloat => TypeKind::Float(NumberType {
                signed: is_signed.unwrap(),
            }),
            TokenType::KwdDouble => TypeKind::Double(NumberType {
                signed: is_signed.unwrap(),
            }),
            _ => unreachable!(),
        };
        debug!("type: {:?}", ty);
        Ok(ty)
    }

    // prefix-declarator :: pointer*
    fn parse_prefix_declarator(&mut self) -> UccResult<Option<Type>> {
        let mut ty: Option<Type> = None;
        while let Some(token) = self.cur_token() {
            match token.ty {
                TokenType::PuncAsterisk => {
                    if let Some(t) = ty.as_mut() {
                        t.ty = TypeKind::Ptr(Box::new(t.clone()));
                    } else {
                        ty = Some(Type::new(TypeKind::Ptr(Box::new(
                            ty.unwrap_or(Type::new(TypeKind::Invalid)),
                        ))));
                    }
                }
                TokenType::KwdConst => {
                    if let Some(t) = ty.as_mut() {
                        t.is_const = true;
                    } else {
                        return self.error("expected pointer before const");
                    }
                }
                TokenType::KwdVolatile => {
                    if let Some(t) = ty.as_mut() {
                        t.is_volatile = true;
                    } else {
                        return self.error("expected pointer before volatile");
                    }
                }
                _ => break,
            }
            self.next_token();
        }
        Ok(ty)
    }

    fn parse_parameter_declaration(&mut self) -> UccResult<VarDecl> {
        let mut decl_type = self.parse_declaration_specifier()?;
        let (name, mut direct_ty) = self.parse_declarator()?;
        let result_ty = match direct_ty.as_mut() {
            Some(t) => match &mut t.ty {
                TypeKind::Ptr(_) | TypeKind::Array(_, _) | TypeKind::Func(_) => {
                    self.fill_in_inner_type(t, decl_type)?;
                    t
                }
                _ => return self.error("expected pointer type"),
            },
            None => &mut decl_type,
        };
        Ok(VarDecl {
            ty: result_ty.clone(),
            name,
            init: None,
        })
    }

    fn parse_parameter_list(&mut self) -> UccResult<Vec<VarDecl>> {
        let mut params = vec![];
        loop {
            if let Some(token) = self.cur_token().cloned() {
                if token.ty == TokenType::PuncRightParen {
                    break;
                }
            } else {
                return self.error("expected ')' or parameter declaration");
            }
            params.push(self.parse_parameter_declaration()?);
            if let Some(token) = self.cur_token().cloned() {
                match token.ty {
                    TokenType::PuncComma => {
                        self.next_token();
                    }
                    TokenType::PuncRightParen => break,
                    _ => return self.error("expected ')' or ','"),
                }
            } else {
                return self.error("expected ')' or ','");
            }
        }
        Ok(params)
    }

    // direct-declarator :: identifier | (declarator) | direct-declarator [constant-expression?] | direct-declarator (parameter-type-list?)
    fn parse_direct_declarator(&mut self) -> UccResult<(String, Option<Type>)> {
        let mut name = None;
        let mut decl_type: Option<Type> = None;
        loop {
            if let Some(token) = self.cur_token().cloned() {
                match &token.ty {
                    TokenType::Identifier(id) => {
                        if let Some(_) = name {
                            return self.error("unexpected identifier");
                        } else {
                            name = Some(id.clone());
                        }
                        self.next_token();
                    }
                    TokenType::PuncLeftParen => {
                        self.next_token();
                        match self.parse_parameter_list() {
                            Ok(param_list) => {
                                self.expect(TokenType::PuncRightParen)?;
                                let func_ty = Type::new(TypeKind::Func(FuncType {
                                    ret: Box::new(Type::new(TypeKind::Invalid)),
                                    params: param_list.iter().map(|p| p.ty.clone()).collect(),
                                }));
                                decl_type = match decl_type.as_mut() {
                                    Some(t) => {
                                        self.fill_in_inner_type(t.borrow_mut(), func_ty)?;
                                        Some(t.clone())
                                    }
                                    None => Some(func_ty),
                                };
                                continue;
                            }
                            Err(e) => {
                                if decl_type.is_some() {
                                    return Err(e);
                                }
                            }
                        }
                        let (id, ty) = self.parse_declarator()?;
                        self.expect(TokenType::PuncRightParen)?;
                        name = Some(id);
                        decl_type = ty;
                    }
                    _ => break,
                }
            } else {
                if name.is_none() && decl_type.is_none() {
                    return self.error("expected identifier or '('");
                }
                break;
            }
        }
        Ok((
            match name {
                Some(x) => x,
                None => "".to_string(),
            },
            decl_type,
        ))
    }

    // declarator :: prefix-declarator direct-declarator
    fn parse_declarator(&mut self) -> UccResult<(String, Option<Type>)> {
        let prefix_ty = self.parse_prefix_declarator()?;
        let (id, direct_ty) = self.parse_direct_declarator()?;
        Ok((
            id,
            match direct_ty {
                Some(mut t) => {
                    if let Some(prefix_ty) = prefix_ty {
                        self.fill_in_inner_type(&mut t, prefix_ty)?;
                    }
                    Some(t)
                }
                None => prefix_ty,
            },
        ))
    }

    fn parse_declaration(&mut self) -> UccResult<Decl> {
        let mut decl_type = self.parse_declaration_specifier()?;
        let (name, mut direct_ty) = self.parse_declarator()?;
        // todo: parse initial value
        let result_ty = match direct_ty.as_mut() {
            Some(t) => match &mut t.ty {
                TypeKind::Ptr(_) | TypeKind::Array(_, _) | TypeKind::Func(_) => {
                    self.fill_in_inner_type(t, decl_type)?;
                    t
                }
                _ => return self.error("expected pointer type"),
            },
            None => &mut decl_type,
        };
        Ok(Decl::Var(Box::new(VarDecl {
            ty: result_ty.clone(),
            name,
            init: None,
        })))
    }

    pub fn parse(&mut self) -> UccResult<Program> {
        let mut decls = vec![];
        while let Some(token) = self.cur_token() {
            if token.ty == TokenType::EOF {
                break;
            }
            let decl = self.parse_declaration()?;
            match &decl {
                Decl::Var(v) => debug!("var: {}", v.to_string()),
                _ => debug!("decl: {:?}", decl),
            }
            decls.push(decl);
        }
        Ok(Program { decls })
    }
}

#[cfg(test)]
mod tests {
    use super::super::lex;
    use super::*;

    fn get_parser(code: &str) -> Parser {
        Parser::new(
            lex::Lexer::new(code)
                .lex()
                .unwrap_or_else(|e| panic!("{:?}", e)),
        )
    }

    fn get_var_decl(code: &str) -> VarDecl {
        let mut parser = get_parser(code);
        match parser.parse_declaration().unwrap() {
            Decl::Var(v) => *v,
            _ => panic!("expected var decl"),
        }
    }

    #[test]
    fn test_parse_declaration() {
        assert_eq!(
            get_var_decl("const int * x(int*(*)(int, double))").to_string(),
            "x is function (pointer to function (int, double) returning pointer to int) returning pointer to const int"
        );
        assert_eq!(get_var_decl("int").to_string(), " is int");
        assert_eq!(
            get_var_decl("const volatile int t").to_string(),
            "t is const volatile int"
        );
        // assert_eq!(
        //     get_var_decl("const int * x[10]").to_string(),
        //     "x is array[10] of pointer to const int"
        // );
        assert_eq!(
            get_var_decl("const int * x").to_string(),
            "x is pointer to const int"
        );
    }
}
