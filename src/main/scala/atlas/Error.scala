package atlas

sealed abstract class Error
final case class ParseError(message: String) extends Error
final case class RuntimeError(message: String, cause: Option[Throwable] = None) extends Error
