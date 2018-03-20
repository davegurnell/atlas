package atlas

import fastparse.all._

import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe.{Expr => _, _}

  def exprMacro(args: Tree *): Tree =
    macroImpl[Ast.Expr](Parser.parsers.exprToEnd, args)

  def stmtMacro(args: Tree *): Tree =
    macroImpl[Ast.Stmt](Parser.parsers.stmtToEnd, args)

  def progMacro(args: Tree *): Tree =
    macroImpl[Ast.Expr](Parser.parsers.progToEnd, args)

  private def macroImpl[A](parser: Parser[A], args: Seq[Tree])(implicit lift: Liftable[A]): Tree = {
    if(args.nonEmpty) {
      c.abort(c.enclosingPosition, "Cannot interpolate in an expression literal")
    } else {
      c.prefix.tree match {
        case q"$_($_(..$partTrees))" =>
          parser.parse(formatCode(partTrees)) match {
            case Parsed.Success(value, _) =>
              lift(value)

            case failure: Parsed.Failure =>
              c.abort(c.enclosingPosition, s"Error parsing atlas code: $failure\n${failure.extra.traced}")
          }
      }
    }
  }

  // Adapted from UnindentMacros.transform(...) in davegurnell/unindent:
  private def formatCode(partTrees: Seq[Tree]): String = {
    import scala.util.matching.Regex.Match

    val prefixRegex = """^\n""".r
    val suffixRegex = """\n[ \t]*$""".r
    val indentRegex = """\n[ \t]+""".r

    val parts = partTrees.map { case Literal(Constant(part: String)) => part }

    val numParts = parts.length

    val minIndent = parts
      .flatMap(indentRegex.findAllIn)
      .map(_.length)
      .foldLeft(Int.MaxValue)(math.min)

    parts.zipWithIndex.map {
      case (part, index) =>
        // De-indent newlines:
        var ans = indentRegex.replaceAllIn(part, (m: Match) =>
          "\n" + (" " * (m.group(0).length - minIndent)))

        // Strip any initial newline from the beginning of the string:
        if(index == 0) {
          ans = prefixRegex.replaceFirstIn(ans, "")
        }

        // Strip any final newline from the end of the string:
        if(index == numParts - 1) {
          ans = suffixRegex.replaceFirstIn(ans, "")
        }

        ans
    }.mkString
  }

  implicit def listLiftable[A](implicit itemLiftable: Liftable[A]): Liftable[List[A]] =
    new Liftable[List[A]] {
      def apply(list: List[A]): Tree =
        q"_root_.scala.List(..${list.map(itemLiftable.apply)})"
    }

  implicit lazy val stmtLiftable: Liftable[Ast.Stmt] =
    new Liftable[Ast.Stmt] {
      def apply(stmt: Ast.Stmt): Tree =
        stmt match {
          case Ast.DefnStmt(name, expr) => q"_root_.atlas.Ast.DefnStmt($name, $expr)"
          case Ast.ExprStmt(expr)       => q"_root_.arlas.Ast.ExprStmt($expr)"
        }
    }

  implicit lazy val exprLiftable: Liftable[Ast.Expr] =
    new Liftable[Ast.Expr] {
      def apply(expr: Ast.Expr): Tree =
        expr match {
          case Ast.Ref(id)                  => q"_root_.atlas.Ast.Ref($id)"
          case Ast.Block(stmts, expr)       => q"_root_.atlas.Ast.Block($stmts, $expr)"
          case Ast.Select(expr, ref)        => q"_root_.atlas.Ast.Select($expr, $ref)"
          case Ast.Cond(test, arm1, arm2)   => q"_root_.atlas.Ast.Cond($test, $arm1, $arm2)"
          case Ast.Infix(op, arg1, arg2)    => q"_root_.atlas.Ast.Infix($op, $arg1, $arg2)"
          case Ast.Prefix(op, arg)          => q"_root_.atlas.Ast.Prefix($op, $arg)"
          case Ast.Apply(func, args)        => q"_root_.atlas.Ast.Apply($func, $args)"
          case Ast.Literal.Func(args, body) => q"_root_.atlas.Ast.Literal.Func($args, $body)"
          case Ast.Literal.Obj(fields)      => q"_root_.atlas.Ast.Literal.Obj($fields)"
          case Ast.Literal.Arr(items)       => q"_root_.atlas.Ast.Literal.Arr($items)"
          case Ast.Literal.Str(value)       => q"_root_.atlas.Ast.Literal.Str($value)"
          case Ast.Literal.Intr(value)      => q"_root_.atlas.Ast.Literal.Intr($value)"
          case Ast.Literal.Real(value)      => q"_root_.atlas.Ast.Literal.Real($value)"
          case Ast.Literal.Null             => q"_root_.atlas.Ast.Literal.Null"
          case Ast.Literal.True             => q"_root_.atlas.Ast.Literal.True"
          case Ast.Literal.False            => q"_root_.atlas.Ast.Literal.False"
        }
    }

  implicit val prefixOpLiftable: Liftable[PrefixOp] =
    new Liftable[PrefixOp] {
      def apply(op: PrefixOp): Tree =
        op match {
          case PrefixOp.Not => q"_root_.atlas.PrefixOp.Not"
          case PrefixOp.Pos => q"_root_.atlas.PrefixOp.Pos"
          case PrefixOp.Neg => q"_root_.atlas.PrefixOp.Neg"
        }
    }

  implicit val infixOpLiftable: Liftable[InfixOp] =
    new Liftable[InfixOp] {
      def apply(op: InfixOp): Tree =
        op match {
          case InfixOp.Add => q"_root_.atlas.InfixOp.Add"
          case InfixOp.Sub => q"_root_.atlas.InfixOp.Sub"
          case InfixOp.Mul => q"_root_.atlas.InfixOp.Mul"
          case InfixOp.Div => q"_root_.atlas.InfixOp.Div"
          case InfixOp.And => q"_root_.atlas.InfixOp.And"
          case InfixOp.Or  => q"_root_.atlas.InfixOp.Or"
          case InfixOp.Eq  => q"_root_.atlas.InfixOp.Eq"
          case InfixOp.Ne  => q"_root_.atlas.InfixOp.Ne"
          case InfixOp.Gt  => q"_root_.atlas.InfixOp.Gt"
          case InfixOp.Lt  => q"_root_.atlas.InfixOp.Lt"
          case InfixOp.Gte => q"_root_.atlas.InfixOp.Gte"
          case InfixOp.Lte => q"_root_.atlas.InfixOp.Lte"
        }
    }

  implicit val fieldPairLiftable: Liftable[(String, Ast.Expr)] =
    new Liftable[(String, Ast.Expr)] {
      def apply(field: (String, Ast.Expr)): Tree = {
        val (name, expr) = field
        q"($name, $expr)"
      }
    }
}
