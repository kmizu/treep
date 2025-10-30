package com.github.kmizu.treep.interpreter

sealed trait TreepRuntimeError extends RuntimeException:
  def message: String
  override def getMessage(): String = message

final case class UndefinedVariable(name: String) extends TreepRuntimeError:
  def message = s"undefined variable: $name"

final case class UndefinedFunction(name: String) extends TreepRuntimeError:
  def message = s"function not found: $name"

final case class ArityMismatch(expected: Int, actual: Int, functionName: Option[String] = None) extends TreepRuntimeError:
  def message = functionName match
    case Some(fn) => s"arity mismatch in $fn: expected $expected argument(s), got $actual"
    case None => s"arity mismatch: expected $expected argument(s), got $actual"

final case class TypeMismatchError(expected: String, actual: String, context: Option[String] = None) extends TreepRuntimeError:
  def message = context match
    case Some(ctx) => s"type mismatch in $ctx: expected $expected, got $actual"
    case None => s"type mismatch: expected $expected, got $actual"

final case class InvalidOperation(operation: String, reason: String) extends TreepRuntimeError:
  def message = s"invalid operation '$operation': $reason"

final case class IndexError(indexable: String, reason: String) extends TreepRuntimeError:
  def message = s"index error on $indexable: $reason"

final case class AssignmentError(reason: String) extends TreepRuntimeError:
  def message = s"assignment error: $reason"

final case class MissingElement(kind: String, context: String) extends TreepRuntimeError:
  def message = s"missing required element '$kind' in $context"

final case class MissingAttribute(key: String, context: String) extends TreepRuntimeError:
  def message = s"missing required attribute '$key' in $context"

final case class DivisionByZero() extends TreepRuntimeError:
  def message = "division by zero"
