package atlas

import fastparse.all._

import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe.{Expr => _, Type => _, TypeRef => _, _}

  def exprMacro(args: Tree *): Tree =
    macroImpl[Expr](Parser.parsers.exprToEnd, args)

  def progMacro(args: Tree *): Tree =
    macroImpl[Expr](Parser.parsers.progToEnd, args)

  private def macroImpl[A](parser: Parser[A], args: Seq[Tree])(implicit lift: Liftable[A]): Tree = {
    if(args.nonEmpty) {
      c.abort(c.enclosingPosition, "String interpolation not supported!")
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

  val pkg = q"_root_.atlas"

  implicit def listLiftable[A](implicit itemLiftable: Liftable[A]): Liftable[List[A]] =
    new Liftable[List[A]] {
      def apply(list: List[A]): Tree =
        q"_root_.scala.List(..${list.map(itemLiftable.apply)})"
    }

  implicit lazy val exprLiftable: Liftable[Expr] =
    new Liftable[Expr] {
      def apply(expr: Expr): Tree =
        expr match {
          case RefExpr(id, loc)                => q"$pkg.RefExpr($id, $loc)"
          case LetExpr(name, expr, loc)        => q"$pkg.LetExpr($name, $expr, $loc)"
          case AppExpr(func, args, loc)        => q"$pkg.AppExpr($func, $args, $loc)"
          case InfixExpr(op, arg1, arg2, loc)  => q"$pkg.InfixExpr($op, $arg1, $arg2, $loc)"
          case PrefixExpr(op, arg, loc)        => q"$pkg.PrefixExpr($op, $arg, $loc)"
          case FuncExpr(args, body, loc)       => q"$pkg.FuncExpr($args, $body, $loc)"
          case ProgExpr(stmts, expr, loc)      => q"$pkg.ProgExpr($stmts, $expr, $loc)"
          case BlockExpr(stmts, expr, loc)     => q"$pkg.BlockExpr($stmts, $expr, $loc)"
          case SelectExpr(expr, ref, loc)      => q"$pkg.SelectExpr($expr, $ref, $loc)"
          case CondExpr(test, arm1, arm2, loc) => q"$pkg.CondExpr($test, $arm1, $arm2, $loc)"
          case ParenExpr(expr, loc)            => q"$pkg.ParenExpr($expr, $loc)"
          case ObjExpr(fields, loc)            => q"$pkg.ObjExpr($fields, $loc)"
          case ArrExpr(items, loc)             => q"$pkg.ArrExpr($items, $loc)"
          case StrExpr(value, loc)             => q"$pkg.StrExpr($value, $loc)"
          case IntExpr(value, loc)             => q"$pkg.IntExpr($value, $loc)"
          case DblExpr(value, loc)             => q"$pkg.DblExpr($value, $loc)"
          case BoolExpr(value, loc)            => q"$pkg.BoolExpr($value, $loc)"
          case NullExpr(loc)                   => q"$pkg.NullExpr($loc)"
        }
    }

  implicit lazy val sourceLocLiftable: Liftable[SourceLoc] =
    new Liftable[SourceLoc] {
      def apply(loc: SourceLoc): Tree =
        q"$pkg.SourceLoc(${loc.start}, ${loc.end})"
    }

  implicit lazy val argLiftable: Liftable[FuncArg] =
    new Liftable[FuncArg] {
      def apply(arg: FuncArg): Tree =
        q"$pkg.FuncArg(${arg.argName})"
    }

  implicit lazy val prefixOpLiftable: Liftable[PrefixOp] =
    new Liftable[PrefixOp] {
      def apply(op: PrefixOp): Tree =
        op match {
          case PrefixOp.Not => q"$pkg.PrefixOp.Not"
          case PrefixOp.Pos => q"$pkg.PrefixOp.Pos"
          case PrefixOp.Neg => q"$pkg.PrefixOp.Neg"
        }
    }

  implicit lazy val infixOpLiftable: Liftable[InfixOp] =
    new Liftable[InfixOp] {
      def apply(op: InfixOp): Tree =
        op match {
          case InfixOp.Add => q"$pkg.InfixOp.Add"
          case InfixOp.Sub => q"$pkg.InfixOp.Sub"
          case InfixOp.Mul => q"$pkg.InfixOp.Mul"
          case InfixOp.Div => q"$pkg.InfixOp.Div"
          case InfixOp.And => q"$pkg.InfixOp.And"
          case InfixOp.Or  => q"$pkg.InfixOp.Or"
          case InfixOp.Eq  => q"$pkg.InfixOp.Eq"
          case InfixOp.Ne  => q"$pkg.InfixOp.Ne"
          case InfixOp.Gt  => q"$pkg.InfixOp.Gt"
          case InfixOp.Lt  => q"$pkg.InfixOp.Lt"
          case InfixOp.Gte => q"$pkg.InfixOp.Gte"
          case InfixOp.Lte => q"$pkg.InfixOp.Lte"
        }
    }

  implicit lazy val fieldPairLiftable: Liftable[(String, Expr)] =
    new Liftable[(String, Expr)] {
      def apply(field: (String, Expr)): Tree = {
        val (name, expr) = field
        q"($name, $expr)"
      }
    }
}
