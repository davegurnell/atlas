package atlas

sealed abstract class Error
final case class ParseError(message: String) extends Error
final case class TypeError(message: String) extends Error
final case class RuntimeError(message: String, cause: Option[Exception] = None) extends Error
