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
          case RefExpr(id)                => q"$pkg.RefExpr($id)"
          case LetExpr(name, tpe, expr)   => q"$pkg.LetExpr($name, $tpe, $expr)"
          case AppExpr(func, args)        => q"$pkg.AppExpr($func, $args)"
          case InfixExpr(op, arg1, arg2)  => q"$pkg.InfixExpr($op, $arg1, $arg2)"
          case PrefixExpr(op, arg)        => q"$pkg.PrefixExpr($op, $arg)"
          case FuncExpr(args, tpe, body)  => q"$pkg.FuncExpr($args, $tpe, $body)"
          case BlockExpr(stmts, expr)     => q"$pkg.BlockExpr($stmts, $expr)"
          // case SelectExpr(expr, ref)   => q"$pkg.SelectExpr($expr, $ref)"
          case CondExpr(test, arm1, arm2) => q"$pkg.CondExpr($test, $arm1, $arm2)"
          case CastExpr(expr, asType)     => q"$pkg.CastExpr($expr, $asType)"
          // case ObjExpr(fields)         => q"$pkg.ObjExpr($fields)"
          case ArrExpr(items)             => q"$pkg.ArrExpr($items)"
          case StrExpr(value)             => q"$pkg.StrExpr($value)"
          case IntExpr(value)             => q"$pkg.IntExpr($value)"
          case DblExpr(value)             => q"$pkg.DblExpr($value)"
          case BoolExpr(value)            => q"$pkg.BoolExpr($value)"
          case NullExpr                   => q"$pkg.NullExpr"
        }
    }

  implicit lazy val argLiftable: Liftable[FuncArg] =
    new Liftable[FuncArg] {
      def apply(arg: FuncArg): Tree =
        q"$pkg.FuncArg(${arg.argName}, ${arg.argType})"
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

  implicit lazy val typeLiftable: Liftable[Type] =
    new Liftable[Type] {
      def apply(tpe: Type): Tree =
        tpe match {
          case TypeVar(id)         => q"$pkg.TypeVar($id)"
          case TypeRef(id)         => q"$pkg.TypeRef($id)"
          case FuncType(args, res) => q"$pkg.FuncType($args, $res)"
          // case UnionType(types)    => q"$pkg.UnionType($types)"
          // case ObjType(fieldTypes) => q"$pkg.ObjType($fieldTypes)"
          case ArrType(tpe)        => q"$pkg.ArrType($tpe)"
          case StrType             => q"$pkg.StrType"
          case IntType             => q"$pkg.IntType"
          case DblType             => q"$pkg.DblType"
          case NullType            => q"$pkg.NullType"
          case BoolType            => q"$pkg.BoolType"
          case AnyType             => q"$pkg.AnyType"
        }
    }

  implicit lazy val fieldPairLiftable: Liftable[(String, Expr)] =
    new Liftable[(String, Expr)] {
      def apply(field: (String, Expr)): Tree = {
        val (name, expr) = field
        q"($name, $expr)"
      }
    }

  implicit lazy val argPairLiftable: Liftable[(String, Type)] =
    new Liftable[(String, Type)] {
      def apply(field: (String, Type)): Tree = {
        val (name, tpe) = field
        q"($name, $tpe)"
      }
    }
}
