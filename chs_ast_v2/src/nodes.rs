use std::rc::Rc;

pub enum Literal {
    Interger(i64),
    Char(char),
    String(String)
}

pub enum Expr {
    SExpr(Rc<Self>, Rc<[Self]>),
    Intrinsic(String),
    While(Rc<[Self]>, Rc<[Self]>),
    If(Rc<[Self]>, Option<Rc<[Self]>>),
    List(Rc<[Self]>),
    Literal(Literal),
    Lambda(Rc<[Self]>)
}

pub enum TopLevel {
    Fn(String, Rc<[Expr]>),
    // Def(String)
}

pub struct Module {
    pub filesource: String,
    pub program: Vec<TopLevel>
}