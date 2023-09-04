use anyhow::{bail, Context as _};
use boa_ast::{
    declaration::{Binding, VarDeclaration, Variable, VariableList},
    expression::{
        literal::{ArrayLiteral, Literal},
        operator::{binary::BinaryOp, Binary},
        Call, Identifier,
    },
    statement::If,
    Expression, Script, Statement, StatementList, StatementListItem,
};
use boa_interner::Sym;

struct Map<'a> {
    interner: &'a boa_interner::Interner,
}

impl Map<'_> {
    fn resolve(&self, sym: Sym) -> anyhow::Result<String> {
        match self.interner.resolve(sym) {
            Some(found) => match found.utf8() {
                Some(s) => Ok(String::from(s)),
                None => String::from_utf16(found.utf16())
                    .context("Interned symbol contains invalid utf-16"), // should this be a panic?
            },
            None => bail!("Couldn't resolve interned symbol - are you using the same interner?"),
        }
    }
}

macro_rules! unsupported {
    ($node:ident) => {
        ::anyhow::bail!("Unsupported node: {:?}", $node)
    };
    ($node:ident, $($arg:tt)*) => {
        ::anyhow::bail!("Unsupported node ({}): {:?}", ::std::format!($($arg)*), $node)
    };
}

impl Map<'_> {
    fn map_script(&mut self, node: &Script) -> anyhow::Result<rhai::AST> {
        let statement_list = node.statements();
        Ok(rhai::AST::new(
            self.map_statement_list(statement_list)?,
            rhai::Module::new(),
        ))
    }
    fn map_statement_list(&mut self, node: &StatementList) -> anyhow::Result<Vec<rhai::Stmt>> {
        let (_strict, statements) = (node.strict(), node.statements());
        statements
            .iter()
            .map(|node| self.map_statement_list_item(node))
            .try_fold(vec![], |mut acc, el| {
                el.map(|el| {
                    acc.extend(el);
                    acc
                })
            })
    }
    fn map_statement_list_item(
        &mut self,
        node: &StatementListItem,
    ) -> anyhow::Result<Vec<rhai::Stmt>> {
        match node {
            StatementListItem::Statement(node) => self.map_statment(node),
            StatementListItem::Declaration(node) => unsupported!(node),
        }
    }
    fn map_statment(&mut self, node: &Statement) -> anyhow::Result<Vec<rhai::Stmt>> {
        match node {
            Statement::Var(node) => self.map_var_declaration(node),
            Statement::Block(_) => unsupported!(node),
            Statement::Empty => unsupported!(node),
            Statement::Expression(_) => unsupported!(node),
            Statement::If(node) => unsupported!(node),
            Statement::DoWhileLoop(_) => unsupported!(node),
            Statement::WhileLoop(_) => unsupported!(node),
            Statement::ForLoop(_) => unsupported!(node),
            Statement::ForInLoop(_) => unsupported!(node),
            Statement::ForOfLoop(_) => unsupported!(node),
            Statement::Switch(_) => unsupported!(node),
            Statement::Continue(_) => unsupported!(node),
            Statement::Break(_) => unsupported!(node),
            Statement::Return(_) => unsupported!(node),
            Statement::Labelled(_) => unsupported!(node),
            Statement::Throw(_) => unsupported!(node),
            Statement::Try(_) => unsupported!(node),
            Statement::With(_) => unsupported!(node),
        }
    }
    fn map_if(&mut self, node: &If) -> anyhow::Result<rhai::Stmt> {
        todo!()
    }
    fn map_var_declaration(&mut self, node: &VarDeclaration) -> anyhow::Result<Vec<rhai::Stmt>> {
        let VarDeclaration(node) = node;
        self.map_variable_list(node)
    }
    fn map_variable_list(&mut self, node: &VariableList) -> anyhow::Result<Vec<rhai::Stmt>> {
        node.as_ref()
            .iter()
            .map(|node| self.map_variable(node))
            .collect()
    }
    fn map_variable(&mut self, node: &Variable) -> anyhow::Result<rhai::Stmt> {
        let (binding, init) = (node.binding(), node.init());
        Ok(rhai::Stmt::Var(
            Box::new((
                self.map_binding(binding)?,
                init.map(|e| self.map_expression(e))
                    .transpose()?
                    .unwrap_or(rhai::Expr::Unit(rhai::Position::NONE)),
                None,
            )),
            rhai::ASTFlags::empty(),
            rhai::Position::NONE,
        ))
    }
    fn map_expression(&mut self, node: &Expression) -> anyhow::Result<rhai::Expr> {
        match node {
            Expression::Identifier(node) => self.map_identifier_to_expr(node),
            Expression::Literal(node) => self.map_literal(node),
            Expression::This => unsupported!(node),
            Expression::ArrayLiteral(node) => self.map_array_literal(node),
            Expression::ObjectLiteral(_) => unsupported!(node),
            Expression::Spread(_) => unsupported!(node),
            Expression::Function(_) => unsupported!(node),
            Expression::ArrowFunction(_) => unsupported!(node),
            Expression::AsyncArrowFunction(_) => unsupported!(node),
            Expression::Generator(_) => unsupported!(node),
            Expression::AsyncFunction(_) => unsupported!(node),
            Expression::AsyncGenerator(_) => unsupported!(node),
            Expression::Class(_) => unsupported!(node),
            Expression::TemplateLiteral(_) => unsupported!(node),
            Expression::PropertyAccess(_) => unsupported!(node),
            Expression::New(_) => unsupported!(node),
            Expression::Call(node) => self.map_call(node),
            Expression::SuperCall(_) => unsupported!(node),
            Expression::ImportCall(_) => unsupported!(node),
            Expression::Optional(_) => unsupported!(node),
            Expression::TaggedTemplate(_) => unsupported!(node),
            Expression::NewTarget => unsupported!(node),
            Expression::ImportMeta => unsupported!(node),
            Expression::Assign(_) => unsupported!(node),
            Expression::Unary(_) => unsupported!(node),
            Expression::Update(_) => unsupported!(node),
            Expression::Binary(node) => self.map_binary(node),
            Expression::BinaryInPrivate(_) => unsupported!(node),
            Expression::Conditional(_) => unsupported!(node),
            Expression::Await(_) => unsupported!(node),
            Expression::Yield(_) => unsupported!(node),
            Expression::Parenthesized(_) => unsupported!(node),
            _ => unsupported!(node),
        }
    }
    fn map_binary(&mut self, node: &Binary) -> anyhow::Result<rhai::Expr> {
        let (lhs, op, rhs) = (node.lhs(), node.op(), node.rhs());
        let op = self.map_binary_op(&op)?;
        Ok(rhai::Expr::FnCall(
            Box::new(rhai::FnCallExpr {
                namespace: rhai::Namespace::NONE,
                name: rhai::ImmutableString::from(op.literal_syntax()),
                hashes: rhai::FnCallHashes::from_hash(0),
                args: smallvec::smallvec![self.map_expression(lhs)?, self.map_expression(rhs)?],
                capture_parent_scope: false,
                op_token: Some(op),
            }),
            rhai::Position::NONE,
        ))
    }
    fn map_binary_op(&mut self, node: &BinaryOp) -> anyhow::Result<rhai::Token> {
        use boa_ast::expression::operator::binary::{
            ArithmeticOp, BitwiseOp, LogicalOp, RelationalOp,
        };
        match node {
            BinaryOp::Arithmetic(op) => Ok(match op {
                ArithmeticOp::Add => rhai::Token::Plus,
                ArithmeticOp::Sub => rhai::Token::Minus,
                ArithmeticOp::Div => rhai::Token::Divide,
                ArithmeticOp::Mul => rhai::Token::Multiply,
                ArithmeticOp::Exp => rhai::Token::PowerOf,
                ArithmeticOp::Mod => rhai::Token::Modulo,
            }),
            BinaryOp::Bitwise(op) => Ok(match op {
                BitwiseOp::And => rhai::Token::And,
                BitwiseOp::Or => rhai::Token::Or,
                BitwiseOp::Xor => rhai::Token::XOr,
                BitwiseOp::Shl => rhai::Token::LeftShift,
                BitwiseOp::Shr => unsupported!(op, "sign-propogating right-shift"),
                BitwiseOp::UShr => rhai::Token::RightShift,
            }),
            BinaryOp::Relational(op) => Ok(match op {
                RelationalOp::Equal => rhai::Token::EqualsTo,
                RelationalOp::NotEqual => rhai::Token::NotEqualsTo,
                RelationalOp::StrictEqual => unsupported!(op),
                RelationalOp::StrictNotEqual => unsupported!(op),
                RelationalOp::GreaterThan => rhai::Token::GreaterThan,
                RelationalOp::GreaterThanOrEqual => rhai::Token::GreaterThanEqualsTo,
                RelationalOp::LessThan => rhai::Token::LessThan,
                RelationalOp::LessThanOrEqual => rhai::Token::LessThanEqualsTo,
                RelationalOp::In => rhai::Token::In,
                RelationalOp::InstanceOf => unsupported!(op),
            }),
            BinaryOp::Logical(op) => Ok(match op {
                LogicalOp::And => rhai::Token::And,
                LogicalOp::Or => rhai::Token::Or,
                LogicalOp::Coalesce => rhai::Token::DoubleQuestion,
            }),
            BinaryOp::Comma => Ok(rhai::Token::Comma),
        }
    }
    fn map_identifier_to_expr(&mut self, node: &Identifier) -> anyhow::Result<rhai::Expr> {
        self.resolve(node.sym()).map(|ident| {
            rhai::Expr::Variable(
                Box::new((
                    None,
                    rhai::Namespace::NONE,
                    0,
                    rhai::ImmutableString::from(ident),
                )),
                None,
                rhai::Position::NONE,
            )
        })
    }
    fn map_call(&mut self, node: &Call) -> anyhow::Result<rhai::Expr> {
        let (function, args) = (node.function(), node.args());
        let Expression::Identifier(identifier) = function else {
            unsupported!(function, "call to non-bare function")
        };
        Ok(rhai::Expr::FnCall(
            Box::new(rhai::FnCallExpr {
                namespace: rhai::Namespace::NONE,
                name: self
                    .resolve(identifier.sym())
                    .map(rhai::ImmutableString::from)?,
                hashes: rhai::FnCallHashes::from_hash(0),
                args: args
                    .iter()
                    .map(|arg| self.map_expression(arg))
                    .collect::<Result<_, _>>()?,
                capture_parent_scope: true,
                op_token: None,
            }),
            rhai::Position::NONE,
        ))
    }
    fn map_array_literal(&mut self, node: &ArrayLiteral) -> anyhow::Result<rhai::Expr> {
        let (has_trailing_comma_spread, items) = (node.has_trailing_comma_spread(), node.as_ref());
        if has_trailing_comma_spread {
            unsupported!(node, "arrays with comma spread")
        }

        Ok(rhai::Expr::Array(
            Box::new(
                items
                    .iter()
                    .map(|maybe_item| {
                        maybe_item
                            .as_ref()
                            .map(|item| self.map_expression(item))
                            .unwrap_or(Ok(rhai::Expr::Unit(rhai::Position::NONE)))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            rhai::Position::NONE,
        ))
    }
    fn map_literal(&mut self, node: &Literal) -> anyhow::Result<rhai::Expr> {
        match node {
            Literal::String(sym) => self
                .resolve(*sym)
                .map(|it| rhai::Expr::StringConstant(it.into(), rhai::Position::NONE)),
            Literal::Num(f) => Ok(rhai::Expr::FloatConstant(
                rhai::FloatWrapper::new(*f),
                rhai::Position::NONE,
            )),
            Literal::Int(i) => Ok(rhai::Expr::IntegerConstant(*i as i64, rhai::Position::NONE)),
            Literal::BigInt(node) => unsupported!(node),
            Literal::Bool(node) => Ok(rhai::Expr::BoolConstant(*node, rhai::Position::NONE)),
            Literal::Null => Ok(rhai::Expr::Unit(rhai::Position::NONE)),
            Literal::Undefined => unsupported!(node),
        }
    }
    fn map_binding(&mut self, node: &Binding) -> anyhow::Result<rhai::Ident> {
        match node {
            Binding::Identifier(node) => self.map_identifier_to_ident(node),
            Binding::Pattern(node) => unsupported!(node),
        }
    }
    fn map_identifier_to_ident(&mut self, node: &Identifier) -> anyhow::Result<rhai::Ident> {
        let sym = node.sym();
        Ok(rhai::Ident {
            name: rhai::ImmutableString::from(self.resolve(sym)?),
            pos: rhai::Position::NONE,
        })
    }
}

fn main() {
    let mut interner = boa_interner::Interner::new();
    let js = boa_parser::Parser::new(boa_parser::Source::from_bytes(include_bytes!(
        "2helps2b-score.js"
    )))
    .parse_script(&mut interner)
    .unwrap();
    dbg!(&js);

    let rhai = rhai::Engine::new()
        .compile(include_str!("2helps2b-score.rhai"))
        .unwrap();
    dbg!(&rhai);

    let mapped = Map {
        interner: &interner,
    }
    .map_script(&js)
    .unwrap();
    dbg!(&mapped);
}
