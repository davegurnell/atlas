package atlas

sealed abstract class Error extends Product with Serializable

final case class ParseError(message: String) extends Error

sealed abstract class TypeError
final case class VariableNotFound(name: String) extends TypeError

final case class RuntimeError(message: String, cause: Option[Throwable] = None) extends Error
