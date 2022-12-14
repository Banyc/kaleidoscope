/// - Learn more: <https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl02.html#the-abstract-syntax-tree-ast>
#[derive(Debug, PartialEq, Clone)]
pub enum ExprAst {
    Number(f64),
    Variable(String),
    Binary {
        op: String,
        lhs: Box<ExprAst>,
        rhs: Box<ExprAst>,
    },
    Call {
        callee: String,
        args: Vec<ExprAst>,
    },
    If {
        cond: Box<ExprAst>,
        then: Box<ExprAst>,
        else_: Box<ExprAst>,
    },
    For {
        var_name: String,
        start: Box<ExprAst>,
        end: Box<ExprAst>,
        step: Option<Box<ExprAst>>,
        body: Box<ExprAst>,
    },
}

/// - Learn more: <https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl02.html#the-abstract-syntax-tree-ast>
#[derive(Debug, PartialEq, Clone)]
pub struct PrototypeAst {
    pub name: String,
    pub args: Vec<String>,
}

/// - Learn more: <https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl02.html#the-abstract-syntax-tree-ast>
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionAst {
    pub prototype: PrototypeAst,
    pub body: ExprAst,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AnyAst {
    Expr(ExprAst),
    Prototype(PrototypeAst),
    Function(FunctionAst),
}
