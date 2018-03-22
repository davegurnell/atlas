package atlas

sealed abstract class Error
final case class ParseError(message: String) extends Error
final case class RuntimeError(message: String, cause: Option[Exception] = None) extends Error

sealed abstract class TypeError extends Error
final case class TypeNotFound(ident: String) extends TypeError
final case class TypeMismatch(expected: Type, actual: Type) extends TypeError
final case class InfixNotDefined(op: InfixOp, arg1: Type, arg2: Type) extends TypeError
final case class PrefixNotDefined(op: PrefixOp, arg: Type) extends TypeError
