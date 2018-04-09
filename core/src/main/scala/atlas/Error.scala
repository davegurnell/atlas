package atlas

sealed abstract class Error extends Product with Serializable

final case class ParseError(message: String) extends Error

sealed abstract class TypeError

object TypeError {
  final case class VariableNotFound(name: String) extends TypeError
  final case class TypeMismatch(to: Type, from: Type) extends TypeError

  def variableNotFound(name: String): TypeError =
    VariableNotFound(name)

  def typeMismatch(to: Type, from: Type): TypeError =
    TypeMismatch(to, from)
}

final case class RuntimeError(message: String, cause: Option[Throwable] = None) extends Error
