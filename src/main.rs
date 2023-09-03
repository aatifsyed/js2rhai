fn main() {}

#[test]
fn test() {
    let mut interner = boa_interner::Interner::new();
    let ast = boa_parser::Parser::new(boa_parser::Source::from_bytes(include_bytes!(
        "2helps2b-score.js"
    )))
    .parse_script(&mut interner)
    .unwrap();
    dbg!(ast);
}

struct MyVisitor<'a> {
    interner: &'a boa_interner::Interner,
}

use boa_ast::{
    declaration::*,
    expression::{
        access::*,
        literal::*,
        operator::{assign::*, *},
        *,
    },
    function::*,
    pattern::*,
    property::*,
    statement::{iteration::*, *},
    visitor::VisitWith as _,
    *,
};
use boa_interner::Sym;
use std::ops::ControlFlow;

impl<'ast, 'a> boa_ast::visitor::Visitor<'ast> for MyVisitor<'a> {
    type BreakTy = ();

    #[doc = concat!("Visits a `",stringify!(Script),"` with this visitor")]
    fn visit_script(&mut self, node: &'ast Script) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Module),"` with this visitor")]
    fn visit_module(&mut self, node: &'ast Module) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(StatementList),"` with this visitor")]
    fn visit_statement_list(&mut self, node: &'ast StatementList) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(StatementListItem),"` with this visitor")]
    fn visit_statement_list_item(
        &mut self,
        node: &'ast StatementListItem,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Statement),"` with this visitor")]
    fn visit_statement(&mut self, node: &'ast Statement) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Declaration),"` with this visitor")]
    fn visit_declaration(&mut self, node: &'ast Declaration) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Function),"` with this visitor")]
    fn visit_function(&mut self, node: &'ast Function) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Generator),"` with this visitor")]
    fn visit_generator(&mut self, node: &'ast Generator) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(AsyncFunction),"` with this visitor")]
    fn visit_async_function(&mut self, node: &'ast AsyncFunction) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(AsyncGenerator),"` with this visitor")]
    fn visit_async_generator(&mut self, node: &'ast AsyncGenerator) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Class),"` with this visitor")]
    fn visit_class(&mut self, node: &'ast Class) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(LexicalDeclaration),"` with this visitor")]
    fn visit_lexical_declaration(
        &mut self,
        node: &'ast LexicalDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Block),"` with this visitor")]
    fn visit_block(&mut self, node: &'ast Block) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(VarDeclaration),"` with this visitor")]
    fn visit_var_declaration(&mut self, node: &'ast VarDeclaration) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Expression),"` with this visitor")]
    fn visit_expression(&mut self, node: &'ast Expression) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(If),"` with this visitor")]
    fn visit_if(&mut self, node: &'ast If) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(DoWhileLoop),"` with this visitor")]
    fn visit_do_while_loop(&mut self, node: &'ast DoWhileLoop) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(WhileLoop),"` with this visitor")]
    fn visit_while_loop(&mut self, node: &'ast WhileLoop) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ForLoop),"` with this visitor")]
    fn visit_for_loop(&mut self, node: &'ast ForLoop) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ForInLoop),"` with this visitor")]
    fn visit_for_in_loop(&mut self, node: &'ast ForInLoop) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ForOfLoop),"` with this visitor")]
    fn visit_for_of_loop(&mut self, node: &'ast ForOfLoop) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Switch),"` with this visitor")]
    fn visit_switch(&mut self, node: &'ast Switch) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Continue),"` with this visitor")]
    fn visit_continue(&mut self, node: &'ast Continue) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Break),"` with this visitor")]
    fn visit_break(&mut self, node: &'ast Break) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Return),"` with this visitor")]
    fn visit_return(&mut self, node: &'ast Return) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Labelled),"` with this visitor")]
    fn visit_labelled(&mut self, node: &'ast Labelled) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Throw),"` with this visitor")]
    fn visit_throw(&mut self, node: &'ast Throw) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Try),"` with this visitor")]
    fn visit_try(&mut self, node: &'ast Try) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(With),"` with this visitor")]
    fn visit_with(&mut self, node: &'ast With) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Identifier),"` with this visitor")]
    fn visit_identifier(&mut self, node: &'ast Identifier) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(FormalParameterList),"` with this visitor")]
    fn visit_formal_parameter_list(
        &mut self,
        node: &'ast FormalParameterList,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ClassElement),"` with this visitor")]
    fn visit_class_element(&mut self, node: &'ast ClassElement) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(PrivateName),"` with this visitor")]
    fn visit_private_name(&mut self, node: &'ast PrivateName) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(VariableList),"` with this visitor")]
    fn visit_variable_list(&mut self, node: &'ast VariableList) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Variable),"` with this visitor")]
    fn visit_variable(&mut self, node: &'ast Variable) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Binding),"` with this visitor")]
    fn visit_binding(&mut self, node: &'ast Binding) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Pattern),"` with this visitor")]
    fn visit_pattern(&mut self, node: &'ast Pattern) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Literal),"` with this visitor")]
    fn visit_literal(&mut self, node: &'ast Literal) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ArrayLiteral),"` with this visitor")]
    fn visit_array_literal(&mut self, node: &'ast ArrayLiteral) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ObjectLiteral),"` with this visitor")]
    fn visit_object_literal(&mut self, node: &'ast ObjectLiteral) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Spread),"` with this visitor")]
    fn visit_spread(&mut self, node: &'ast Spread) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ArrowFunction),"` with this visitor")]
    fn visit_arrow_function(&mut self, node: &'ast ArrowFunction) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(AsyncArrowFunction),"` with this visitor")]
    fn visit_async_arrow_function(
        &mut self,
        node: &'ast AsyncArrowFunction,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(TemplateLiteral),"` with this visitor")]
    fn visit_template_literal(
        &mut self,
        node: &'ast TemplateLiteral,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(PropertyAccess),"` with this visitor")]
    fn visit_property_access(&mut self, node: &'ast PropertyAccess) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(New),"` with this visitor")]
    fn visit_new(&mut self, node: &'ast New) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Call),"` with this visitor")]
    fn visit_call(&mut self, node: &'ast Call) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(SuperCall),"` with this visitor")]
    fn visit_super_call(&mut self, node: &'ast SuperCall) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ImportCall),"` with this visitor")]
    fn visit_import_call(&mut self, node: &'ast ImportCall) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Optional),"` with this visitor")]
    fn visit_optional(&mut self, node: &'ast Optional) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(TaggedTemplate),"` with this visitor")]
    fn visit_tagged_template(&mut self, node: &'ast TaggedTemplate) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Assign),"` with this visitor")]
    fn visit_assign(&mut self, node: &'ast Assign) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Unary),"` with this visitor")]
    fn visit_unary(&mut self, node: &'ast Unary) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Update),"` with this visitor")]
    fn visit_update(&mut self, node: &'ast Update) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Binary),"` with this visitor")]
    fn visit_binary(&mut self, node: &'ast Binary) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(BinaryInPrivate),"` with this visitor")]
    fn visit_binary_in_private(
        &mut self,
        node: &'ast BinaryInPrivate,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Conditional),"` with this visitor")]
    fn visit_conditional(&mut self, node: &'ast Conditional) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Await),"` with this visitor")]
    fn visit_await(&mut self, node: &'ast Await) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Yield),"` with this visitor")]
    fn visit_yield(&mut self, node: &'ast Yield) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Parenthesized),"` with this visitor")]
    fn visit_parenthesized(&mut self, node: &'ast Parenthesized) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ForLoopInitializer),"` with this visitor")]
    fn visit_for_loop_initializer(
        &mut self,
        node: &'ast ForLoopInitializer,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(IterableLoopInitializer),"` with this visitor")]
    fn visit_iterable_loop_initializer(
        &mut self,
        node: &'ast IterableLoopInitializer,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Case),"` with this visitor")]
    fn visit_case(&mut self, node: &'ast Case) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Sym),"` with this visitor")]
    fn visit_sym(&mut self, node: &'ast Sym) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(LabelledItem),"` with this visitor")]
    fn visit_labelled_item(&mut self, node: &'ast LabelledItem) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Catch),"` with this visitor")]
    fn visit_catch(&mut self, node: &'ast Catch) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(Finally),"` with this visitor")]
    fn visit_finally(&mut self, node: &'ast Finally) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(FormalParameter),"` with this visitor")]
    fn visit_formal_parameter(
        &mut self,
        node: &'ast FormalParameter,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(PropertyName),"` with this visitor")]
    fn visit_property_name(&mut self, node: &'ast PropertyName) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(MethodDefinition),"` with this visitor")]
    fn visit_method_definition(
        &mut self,
        node: &'ast MethodDefinition,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ObjectPattern),"` with this visitor")]
    fn visit_object_pattern(&mut self, node: &'ast ObjectPattern) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ArrayPattern),"` with this visitor")]
    fn visit_array_pattern(&mut self, node: &'ast ArrayPattern) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(PropertyDefinition),"` with this visitor")]
    fn visit_property_definition(
        &mut self,
        node: &'ast PropertyDefinition,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(TemplateElement),"` with this visitor")]
    fn visit_template_element(
        &mut self,
        node: &'ast TemplateElement,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(SimplePropertyAccess),"` with this visitor")]
    fn visit_simple_property_access(
        &mut self,
        node: &'ast SimplePropertyAccess,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(PrivatePropertyAccess),"` with this visitor")]
    fn visit_private_property_access(
        &mut self,
        node: &'ast PrivatePropertyAccess,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(SuperPropertyAccess),"` with this visitor")]
    fn visit_super_property_access(
        &mut self,
        node: &'ast SuperPropertyAccess,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(OptionalOperation),"` with this visitor")]
    fn visit_optional_operation(
        &mut self,
        node: &'ast OptionalOperation,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(AssignTarget),"` with this visitor")]
    fn visit_assign_target(&mut self, node: &'ast AssignTarget) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ObjectPatternElement),"` with this visitor")]
    fn visit_object_pattern_element(
        &mut self,
        node: &'ast ObjectPatternElement,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ArrayPatternElement),"` with this visitor")]
    fn visit_array_pattern_element(
        &mut self,
        node: &'ast ArrayPatternElement,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(PropertyAccessField),"` with this visitor")]
    fn visit_property_access_field(
        &mut self,
        node: &'ast PropertyAccessField,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(OptionalOperationKind),"` with this visitor")]
    fn visit_optional_operation_kind(
        &mut self,
        node: &'ast OptionalOperationKind,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ModuleItemList),"` with this visitor")]
    fn visit_module_item_list(&mut self, node: &'ast ModuleItemList) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ModuleItem),"` with this visitor")]
    fn visit_module_item(&mut self, node: &'ast ModuleItem) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ModuleSpecifier),"` with this visitor")]
    fn visit_module_specifier(
        &mut self,
        node: &'ast ModuleSpecifier,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ImportKind),"` with this visitor")]
    fn visit_import_kind(&mut self, node: &'ast ImportKind) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ImportDeclaration),"` with this visitor")]
    fn visit_import_declaration(
        &mut self,
        node: &'ast ImportDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ImportSpecifier),"` with this visitor")]
    fn visit_import_specifier(
        &mut self,
        node: &'ast ImportSpecifier,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ReExportKind),"` with this visitor")]
    fn visit_re_export_kind(&mut self, node: &'ast ReExportKind) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ExportDeclaration),"` with this visitor")]
    fn visit_export_declaration(
        &mut self,
        node: &'ast ExportDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    #[doc = concat!("Visits a `",stringify!(ExportSpecifier),"` with this visitor")]
    fn visit_export_specifier(
        &mut self,
        node: &'ast ExportSpecifier,
    ) -> ControlFlow<Self::BreakTy> {
        node.visit_with(self)
    }

    fn visit<N: Into<boa_ast::visitor::NodeRef<'ast>>>(
        &mut self,
        node: N,
    ) -> std::ops::ControlFlow<Self::BreakTy> {
        let node = node.into();
        match node {
            boa_ast::visitor::NodeRef::Script(n) => self.visit_script(n),
            boa_ast::visitor::NodeRef::Module(n) => self.visit_module(n),
            boa_ast::visitor::NodeRef::StatementList(n) => self.visit_statement_list(n),
            boa_ast::visitor::NodeRef::StatementListItem(n) => self.visit_statement_list_item(n),
            boa_ast::visitor::NodeRef::Statement(n) => self.visit_statement(n),
            boa_ast::visitor::NodeRef::Declaration(n) => self.visit_declaration(n),
            boa_ast::visitor::NodeRef::Function(n) => self.visit_function(n),
            boa_ast::visitor::NodeRef::Generator(n) => self.visit_generator(n),
            boa_ast::visitor::NodeRef::AsyncFunction(n) => self.visit_async_function(n),
            boa_ast::visitor::NodeRef::AsyncGenerator(n) => self.visit_async_generator(n),
            boa_ast::visitor::NodeRef::Class(n) => self.visit_class(n),
            boa_ast::visitor::NodeRef::LexicalDeclaration(n) => self.visit_lexical_declaration(n),
            boa_ast::visitor::NodeRef::Block(n) => self.visit_block(n),
            boa_ast::visitor::NodeRef::VarDeclaration(n) => self.visit_var_declaration(n),
            boa_ast::visitor::NodeRef::Expression(n) => self.visit_expression(n),
            boa_ast::visitor::NodeRef::If(n) => self.visit_if(n),
            boa_ast::visitor::NodeRef::DoWhileLoop(n) => self.visit_do_while_loop(n),
            boa_ast::visitor::NodeRef::WhileLoop(n) => self.visit_while_loop(n),
            boa_ast::visitor::NodeRef::ForLoop(n) => self.visit_for_loop(n),
            boa_ast::visitor::NodeRef::ForInLoop(n) => self.visit_for_in_loop(n),
            boa_ast::visitor::NodeRef::ForOfLoop(n) => self.visit_for_of_loop(n),
            boa_ast::visitor::NodeRef::Switch(n) => self.visit_switch(n),
            boa_ast::visitor::NodeRef::Continue(n) => self.visit_continue(n),
            boa_ast::visitor::NodeRef::Break(n) => self.visit_break(n),
            boa_ast::visitor::NodeRef::Return(n) => self.visit_return(n),
            boa_ast::visitor::NodeRef::Labelled(n) => self.visit_labelled(n),
            boa_ast::visitor::NodeRef::With(n) => self.visit_with(n),
            boa_ast::visitor::NodeRef::Throw(n) => self.visit_throw(n),
            boa_ast::visitor::NodeRef::Try(n) => self.visit_try(n),
            boa_ast::visitor::NodeRef::Identifier(n) => self.visit_identifier(n),
            boa_ast::visitor::NodeRef::FormalParameterList(n) => {
                self.visit_formal_parameter_list(n)
            }
            boa_ast::visitor::NodeRef::ClassElement(n) => self.visit_class_element(n),
            boa_ast::visitor::NodeRef::PrivateName(n) => self.visit_private_name(n),
            boa_ast::visitor::NodeRef::VariableList(n) => self.visit_variable_list(n),
            boa_ast::visitor::NodeRef::Variable(n) => self.visit_variable(n),
            boa_ast::visitor::NodeRef::Binding(n) => self.visit_binding(n),
            boa_ast::visitor::NodeRef::Pattern(n) => self.visit_pattern(n),
            boa_ast::visitor::NodeRef::Literal(n) => self.visit_literal(n),
            boa_ast::visitor::NodeRef::ArrayLiteral(n) => self.visit_array_literal(n),
            boa_ast::visitor::NodeRef::ObjectLiteral(n) => self.visit_object_literal(n),
            boa_ast::visitor::NodeRef::Spread(n) => self.visit_spread(n),
            boa_ast::visitor::NodeRef::ArrowFunction(n) => self.visit_arrow_function(n),
            boa_ast::visitor::NodeRef::AsyncArrowFunction(n) => self.visit_async_arrow_function(n),
            boa_ast::visitor::NodeRef::TemplateLiteral(n) => self.visit_template_literal(n),
            boa_ast::visitor::NodeRef::PropertyAccess(n) => self.visit_property_access(n),
            boa_ast::visitor::NodeRef::New(n) => self.visit_new(n),
            boa_ast::visitor::NodeRef::Call(n) => self.visit_call(n),
            boa_ast::visitor::NodeRef::SuperCall(n) => self.visit_super_call(n),
            boa_ast::visitor::NodeRef::ImportCall(n) => self.visit_import_call(n),
            boa_ast::visitor::NodeRef::Optional(n) => self.visit_optional(n),
            boa_ast::visitor::NodeRef::TaggedTemplate(n) => self.visit_tagged_template(n),
            boa_ast::visitor::NodeRef::Assign(n) => self.visit_assign(n),
            boa_ast::visitor::NodeRef::Unary(n) => self.visit_unary(n),
            boa_ast::visitor::NodeRef::Update(n) => self.visit_update(n),
            boa_ast::visitor::NodeRef::Binary(n) => self.visit_binary(n),
            boa_ast::visitor::NodeRef::BinaryInPrivate(n) => self.visit_binary_in_private(n),
            boa_ast::visitor::NodeRef::Conditional(n) => self.visit_conditional(n),
            boa_ast::visitor::NodeRef::Await(n) => self.visit_await(n),
            boa_ast::visitor::NodeRef::Yield(n) => self.visit_yield(n),
            boa_ast::visitor::NodeRef::Parenthesized(n) => self.visit_parenthesized(n),
            boa_ast::visitor::NodeRef::ForLoopInitializer(n) => self.visit_for_loop_initializer(n),
            boa_ast::visitor::NodeRef::IterableLoopInitializer(n) => {
                self.visit_iterable_loop_initializer(n)
            }
            boa_ast::visitor::NodeRef::Case(n) => self.visit_case(n),
            boa_ast::visitor::NodeRef::Sym(n) => self.visit_sym(n),
            boa_ast::visitor::NodeRef::LabelledItem(n) => self.visit_labelled_item(n),
            boa_ast::visitor::NodeRef::Catch(n) => self.visit_catch(n),
            boa_ast::visitor::NodeRef::Finally(n) => self.visit_finally(n),
            boa_ast::visitor::NodeRef::FormalParameter(n) => self.visit_formal_parameter(n),
            boa_ast::visitor::NodeRef::PropertyName(n) => self.visit_property_name(n),
            boa_ast::visitor::NodeRef::MethodDefinition(n) => self.visit_method_definition(n),
            boa_ast::visitor::NodeRef::ObjectPattern(n) => self.visit_object_pattern(n),
            boa_ast::visitor::NodeRef::ArrayPattern(n) => self.visit_array_pattern(n),
            boa_ast::visitor::NodeRef::PropertyDefinition(n) => self.visit_property_definition(n),
            boa_ast::visitor::NodeRef::TemplateElement(n) => self.visit_template_element(n),
            boa_ast::visitor::NodeRef::SimplePropertyAccess(n) => {
                self.visit_simple_property_access(n)
            }
            boa_ast::visitor::NodeRef::PrivatePropertyAccess(n) => {
                self.visit_private_property_access(n)
            }
            boa_ast::visitor::NodeRef::SuperPropertyAccess(n) => {
                self.visit_super_property_access(n)
            }
            boa_ast::visitor::NodeRef::OptionalOperation(n) => self.visit_optional_operation(n),
            boa_ast::visitor::NodeRef::AssignTarget(n) => self.visit_assign_target(n),
            boa_ast::visitor::NodeRef::ObjectPatternElement(n) => {
                self.visit_object_pattern_element(n)
            }
            boa_ast::visitor::NodeRef::ArrayPatternElement(n) => {
                self.visit_array_pattern_element(n)
            }
            boa_ast::visitor::NodeRef::PropertyAccessField(n) => {
                self.visit_property_access_field(n)
            }
            boa_ast::visitor::NodeRef::OptionalOperationKind(n) => {
                self.visit_optional_operation_kind(n)
            }
            boa_ast::visitor::NodeRef::ModuleItemList(n) => self.visit_module_item_list(n),
            boa_ast::visitor::NodeRef::ModuleItem(n) => self.visit_module_item(n),
            boa_ast::visitor::NodeRef::ModuleSpecifier(n) => self.visit_module_specifier(n),
            boa_ast::visitor::NodeRef::ImportKind(n) => self.visit_import_kind(n),
            boa_ast::visitor::NodeRef::ImportDeclaration(n) => self.visit_import_declaration(n),
            boa_ast::visitor::NodeRef::ImportSpecifier(n) => self.visit_import_specifier(n),
            boa_ast::visitor::NodeRef::ReExportKind(n) => self.visit_re_export_kind(n),
            boa_ast::visitor::NodeRef::ExportDeclaration(n) => self.visit_export_declaration(n),
            boa_ast::visitor::NodeRef::ExportSpecifier(n) => self.visit_export_specifier(n),
        }
    }
}
