use anyhow::{bail, Context};
use boa_ast::{
    declaration::{Binding, VarDeclaration, Variable, VariableList},
    expression::{literal::Literal, Identifier},
    Expression, Script, Statement, StatementList, StatementListItem,
};
use boa_interner::Sym;
use proc_macro2::Span;
use syn::Token;

struct Map<'a> {
    interner: &'a boa_interner::Interner,
}

enum Todo {}

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
}

impl Map<'_> {
    fn map_script(&mut self, node: &Script) -> anyhow::Result<syn::Block> {
        Ok(syn::Block {
            brace_token: syn::token::Brace::default(),
            stmts: self.map_statement_list(node.statements())?,
        })
    }
    fn map_statement_list(&mut self, node: &StatementList) -> anyhow::Result<Vec<syn::Stmt>> {
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
    ) -> anyhow::Result<Vec<syn::Stmt>> {
        match node {
            StatementListItem::Statement(node) => self.map_statment(node),
            StatementListItem::Declaration(node) => unsupported!(node),
        }
    }
    fn map_statment(&mut self, node: &Statement) -> anyhow::Result<Vec<syn::Stmt>> {
        match node {
            Statement::Var(node) => self
                .map_var_declaration(node)
                .map(|it| it.into_iter().map(syn::Stmt::Local).collect()),
            Statement::Block(_) => unsupported!(node),
            Statement::Empty => unsupported!(node),
            Statement::Expression(_) => unsupported!(node),
            Statement::If(_) => unsupported!(node),
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
    fn map_var_declaration(&mut self, node: &VarDeclaration) -> anyhow::Result<Vec<syn::Local>> {
        let VarDeclaration(node) = node;
        self.map_variable_list(node)
    }
    fn map_variable_list(&mut self, node: &VariableList) -> anyhow::Result<Vec<syn::Local>> {
        node.as_ref()
            .iter()
            .map(|node| self.map_variable(node))
            .collect()
    }
    fn map_variable(&mut self, node: &Variable) -> anyhow::Result<syn::Local> {
        let (binding, init) = (node.binding(), node.init());
        Ok(syn::Local {
            attrs: vec![],
            let_token: Token![let](Span::call_site()),
            pat: self.map_binding(binding)?,
            init: init
                .map(|node| self.map_expression(node))
                .transpose()?
                .map(|expr| syn::LocalInit {
                    eq_token: Token![=](Span::call_site()),
                    expr: Box::new(expr),
                    diverge: None,
                }),
            semi_token: Token![;](Span::call_site()),
        })
    }
    fn map_expression(&mut self, node: &Expression) -> anyhow::Result<syn::Expr> {
        match node {
            Expression::Identifier(node) => self.map_identifier(node).map(|ident| {
                syn::Expr::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path::from(ident),
                })
            }),
            Expression::Literal(node) => self
                .map_literal(node)
                .map(|lit| syn::Expr::Lit(syn::ExprLit { attrs: vec![], lit })),
            Expression::This => unsupported!(node),
            Expression::ArrayLiteral(_) => unsupported!(node),
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
            Expression::Call(_) => unsupported!(node),
            Expression::SuperCall(_) => unsupported!(node),
            Expression::ImportCall(_) => unsupported!(node),
            Expression::Optional(_) => unsupported!(node),
            Expression::TaggedTemplate(_) => unsupported!(node),
            Expression::NewTarget => unsupported!(node),
            Expression::ImportMeta => unsupported!(node),
            Expression::Assign(_) => unsupported!(node),
            Expression::Unary(_) => unsupported!(node),
            Expression::Update(_) => unsupported!(node),
            Expression::Binary(_) => unsupported!(node),
            Expression::BinaryInPrivate(_) => unsupported!(node),
            Expression::Conditional(_) => unsupported!(node),
            Expression::Await(_) => unsupported!(node),
            Expression::Yield(_) => unsupported!(node),
            Expression::Parenthesized(_) => unsupported!(node),
            _ => unsupported!(node),
        }
    }
    fn map_literal(&mut self, node: &Literal) -> anyhow::Result<syn::Lit> {
        match node {
            Literal::String(sym) => self
                .resolve(*sym)
                .map(|it| syn::LitStr::new(&it, Span::call_site()))
                .map(syn::Lit::Str),
            Literal::Num(f) => Ok(syn::Lit::Float(syn::LitFloat::new(
                &f.to_string(),
                Span::call_site(),
            ))),
            Literal::Int(i) => Ok(syn::Lit::Int(syn::LitInt::new(
                &i.to_string(),
                Span::call_site(),
            ))),
            Literal::BigInt(node) => unsupported!(node),
            Literal::Bool(node) => Ok(syn::Lit::Bool(syn::LitBool::new(*node, Span::call_site()))),
            Literal::Null => unsupported!(node),
            Literal::Undefined => unsupported!(node),
        }
    }
    fn map_binding(&mut self, node: &Binding) -> anyhow::Result<syn::Pat> {
        match node {
            Binding::Identifier(node) => Ok(syn::Pat::Ident(syn::PatIdent {
                attrs: vec![],
                by_ref: None,
                mutability: Some(Token![mut](Span::call_site())),
                ident: self.map_identifier(node)?,
                subpat: None,
            })),
            Binding::Pattern(node) => unsupported!(node),
        }
    }
    fn map_identifier(&mut self, node: &Identifier) -> anyhow::Result<syn::Ident> {
        let sym = node.sym();
        syn::parse_str(&self.resolve(sym)?).context("invalid identifier")
    }
}

fn main() {
    let mut interner = boa_interner::Interner::new();
    let ast = boa_parser::Parser::new(boa_parser::Source::from_bytes(include_bytes!(
        "2helps2b-score.js"
    )))
    .parse_script(&mut interner)
    .unwrap();
    dbg!(ast);
}

#[test]
fn test() {
    let mut interner = boa_interner::Interner::new();
    let ast = boa_parser::Parser::new(boa_parser::Source::from_bytes("var foo = 1;"))
        .parse_script(&mut interner)
        .unwrap();
    let ast = dbg!(ast);
    let mapped = Map {
        interner: &interner,
    }
    .map_script(&ast)
    .unwrap();
    dbg!(mapped);
}
