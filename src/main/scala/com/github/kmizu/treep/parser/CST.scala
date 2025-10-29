package com.github.kmizu.treep.parser

object CST:
  final case class Span(file: String, line: Int, col: Int)

  final case class Program(tops: List[TopDecl])

  sealed trait TopDecl { val span: Option[Span] }
  final case class FunDef(name: String, params: List[Param], returnType: Option[TypeAnnot], body: Block, span: Option[Span] = None) extends TopDecl
  final case class ConstDecl(name: String, annot: Option[TypeAnnot], init: Expr, span: Option[Span] = None) extends TopDecl
  final case class ModuleDecl(name: String, body: Block, span: Option[Span] = None) extends TopDecl
  final case class StructDef(name: String, fields: List[(String, TypeAnnot)], span: Option[Span] = None) extends TopDecl

  final case class Param(name: String, tpe: TypeAnnot)
  final case class TypeAnnot(name: String, args: List[TypeAnnot] = Nil) // Simple or applied

  final case class Block(stmts: List[Stmt], span: Option[Span] = None)

  sealed trait Stmt { val span: Option[Span] }
  final case class LetStmt(name: String, annot: Option[TypeAnnot], init: Expr, span: Option[Span] = None) extends Stmt
  final case class ReturnStmt(expr: Expr, span: Option[Span] = None) extends Stmt
  final case class IfStmt(cond: Expr, thenBlk: Block, elseBlk: Option[Block], span: Option[Span] = None) extends Stmt
  final case class WhileStmt(cond: Expr, body: Block, span: Option[Span] = None) extends Stmt
  final case class ForInStmt(name: String, iterable: Expr, body: Block, span: Option[Span] = None) extends Stmt
  final case class ExprStmt(expr: Expr, span: Option[Span] = None) extends Stmt
  final case class MatchStmt(target: Expr, cases: List[Case], span: Option[Span] = None) extends Stmt

  final case class Case(pattern: Pattern, body: Block)
  sealed trait Pattern
  final case class PInt(value: Int) extends Pattern
  final case class PStr(value: String) extends Pattern
  final case class PBool(value: Boolean) extends Pattern
  final case class PVar(name: String) extends Pattern
  case object PWildcard extends Pattern

  sealed trait Expr
  final case class IntLit(value: Int) extends Expr
  final case class StrLit(value: String) extends Expr
  final case class BoolLit(value: Boolean) extends Expr
  final case class Var(name: String) extends Expr
  final case class Call(name: String, args: List[Expr]) extends Expr
  final case class MethodCall(recv: Expr, name: String, args: List[Expr]) extends Expr
  final case class Index(target: Expr, key: Expr) extends Expr
  final case class Field(target: Expr, name: String) extends Expr
  final case class Unary(op: String, expr: Expr) extends Expr
  final case class Binary(op: String, left: Expr, right: Expr) extends Expr
  final case class ListLit(elems: List[Expr]) extends Expr
  final case class DictLit(pairs: List[(Expr, Expr)]) extends Expr
  final case class Group(expr: Expr) extends Expr
  final case class Lambda(params: List[Param], body: Block) extends Expr
