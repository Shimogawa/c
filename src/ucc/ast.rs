#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumberType {
    pub signed: bool,
}

// pub struct StructType {
//     align: usize,
//     size: usize,
// }

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncType {
    pub ret: Box<Type>,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Invalid,
    Void,
    Bool,
    Char(NumberType),
    Short(NumberType),
    Int(NumberType),
    Long(NumberType),
    Float(NumberType),
    Double(NumberType),
    // Struct,
    // Union,
    // Enum,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
    Func(FuncType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub ty: TypeKind,
    pub is_const: bool,
    pub is_volatile: bool,
    pub is_restrict: bool,
}

impl Type {
    pub fn new(ty: TypeKind) -> Self {
        Self {
            ty,
            is_const: false,
            is_volatile: false,
            is_restrict: false,
        }
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        let mut s = String::new();
        if self.is_const {
            s.push_str("const ");
        }
        if self.is_volatile {
            s.push_str("volatile ");
        }
        if self.is_restrict {
            s.push_str("restrict ");
        }
        match &self.ty {
            TypeKind::Invalid => s.push_str("invalid"),
            TypeKind::Void => s.push_str("void"),
            TypeKind::Bool => s.push_str("bool"),
            TypeKind::Char(_) => s.push_str("char"),
            TypeKind::Short(_) => s.push_str("short"),
            TypeKind::Int(_) => s.push_str("int"),
            TypeKind::Long(_) => s.push_str("long"),
            TypeKind::Float(_) => s.push_str("float"),
            TypeKind::Double(_) => s.push_str("double"),
            TypeKind::Ptr(ty) => s.push_str(&format!("pointer to {}", ty.to_string())),
            TypeKind::Array(ty, len) => {
                s.push_str(&format!("array [{}] of {}", len, ty.to_string()))
            }
            TypeKind::Func(func_ty) => {
                s.push_str("function (");
                for (i, param) in func_ty.params.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&param.to_string());
                }
                s.push_str(&format!(") returning {}", func_ty.ret.to_string()));
            }
        }
        s
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub ty: Type,
    pub init: Option<Expr>,
}

impl ToString for VarDecl {
    fn to_string(&self) -> String {
        format!("{} is {}", self.name, self.ty.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct FuncDecl {
    pub name: String,
    pub ty: Type,
    pub params: Vec<VarDecl>,
    pub body: Option<BlockStmt>,
}

#[derive(Clone, Debug)]
pub enum Decl {
    Var(Box<VarDecl>),
    Func(Box<FuncDecl>),
    // Struct,
    // Union,
    // Enum,
    // Typedef,
}

#[derive(Clone, Debug)]
pub enum ConstExpr {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub func: Expr,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ArrIdxExpr {
    pub arr: Expr,
    pub idx: Expr,
}

#[derive(Clone, Debug)]
pub struct MemberAccessExpr {
    pub obj: Expr,
    pub ident: String,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub expr: Expr,
    pub to_type: Type,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct TernaryExpr {
    pub cond: Expr,
    pub then: Expr,
    pub els: Expr,
}

#[derive(Clone, Debug)]
pub struct AssignmentExpr {
    pub dest: UnaryExpr,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct CommaExpr {
    pub cur: Expr,
    pub next: Expr,
}

#[derive(Clone, Debug)]
pub enum ExprType {
    // 1
    Var(String),
    Const(ConstExpr),
    Call(Box<CallExpr>),                    // ()
    ArrIdx(Box<ArrIdxExpr>),                // []
    MemberAccess(Box<MemberAccessExpr>),    // .
    PtrMemberAccess(Box<MemberAccessExpr>), // ->
    // unary
    // 2
    Inc(Box<UnaryExpr>),        // ++
    Dec(Box<UnaryExpr>),        // --
    Addr(Box<UnaryExpr>),       // &
    Deref(Box<UnaryExpr>),      // *
    Sizeof(Box<UnaryExpr>),     // sizeof
    UnaryPlus(Box<UnaryExpr>),  // +
    UnaryMinus(Box<UnaryExpr>), // -
    Not(Box<UnaryExpr>),        // !
    BitNot(Box<UnaryExpr>),     // ~
    Cast(Box<CastExpr>),        // (type)
    // binary
    // 3
    Mul(Box<BinaryExpr>), // *
    Div(Box<BinaryExpr>), // /
    Mod(Box<BinaryExpr>), // %
    // 4
    Add(Box<BinaryExpr>), // +
    Sub(Box<BinaryExpr>), // -
    // 5
    LShift(Box<BinaryExpr>), // <<
    RShift(Box<BinaryExpr>), // >>
    // 6
    Lt(Box<BinaryExpr>), // <
    Gt(Box<BinaryExpr>), // >
    Le(Box<BinaryExpr>), // <=
    Ge(Box<BinaryExpr>), // >=
    // 7
    Eq(Box<BinaryExpr>), // ==
    Ne(Box<BinaryExpr>), // !=
    // 8
    BitAnd(Box<BinaryExpr>), // &
    // 9
    BitXor(Box<BinaryExpr>), // ^
    // 10
    BitOr(Box<BinaryExpr>), // |
    // 11
    And(Box<BinaryExpr>), // &&
    // 12
    Or(Box<BinaryExpr>), // ||
    // ternary
    // 13
    Cond(Box<TernaryExpr>), // ?:
    // assignment
    // 14
    Assign(Box<AssignmentExpr>),       // =
    MulAssign(Box<AssignmentExpr>),    // *=
    DivAssign(Box<AssignmentExpr>),    // /=
    ModAssign(Box<AssignmentExpr>),    // %=
    AddAssign(Box<AssignmentExpr>),    // +=
    SubAssign(Box<AssignmentExpr>),    // -=
    LShiftAssign(Box<AssignmentExpr>), // <<=
    RShiftAssign(Box<AssignmentExpr>), // >>=
    BitAndAssign(Box<AssignmentExpr>), // &=
    BitXorAssign(Box<AssignmentExpr>), // ^=
    BitOrAssign(Box<AssignmentExpr>),  // |=
    // comma
    // 15
    Comma(Box<CommaExpr>), // ,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub expr: ExprType,
    pub sem_type: Option<Type>,
}

impl Expr {
    pub fn new(expr: ExprType) -> Self {
        Self {
            expr,
            sem_type: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Stmt,
    pub els: Option<Stmt>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Block(Box<BlockStmt>),
    // declaration
    LocalDecl(Box<Decl>),
    // if
    If(Box<IfStmt>),
    // switch case
    // Switch,
    // Case,
    // Default,
    // loops
    While,
    Do,
    For,
    // control
    Continue,
    Break,
    Goto,
    Return,
    // label
    Label,
}
