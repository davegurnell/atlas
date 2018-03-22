package atlas

import Ast.Expr

final case class TypeScope(var bindings: Map[Expr, Type], var variables: Set[Type.Var]) {
  def get(expr: Expr): Option[Type] =
    bindings.collectFirst { case (`expr`, tpe) => tpe }

  def set(expr: Expr, tpe: Type): TypeScope =
    TypeScope(bindings + ((expr, tpe)), variables)

  def gen: (Type.Var, TypeScope) = {
    val variable = TypeEnv.genVar
    (variable, TypeScope(bindings, variables + variable))
  }

  def destructiveSet(expr: Expr, tpe: Type): Unit =
    bindings = bindings + ((expr, tpe))

  def destructiveSetAll(bindings: Seq[(Expr, Type)]): Unit =
    bindings.foreach { case (expr, tpe) => destructiveSet(expr, tpe) }
}

object TypeScope {
  def create: TypeScope =
    TypeScope(Map(), Set())
}

final case class TypeEnv(scopes: List[TypeScope]) {
  def get(expr: Expr): Option[Type] = {
    def loop(scopes: List[TypeScope]): Option[Type] =
      scopes match {
        case head :: tail => head.get(expr).orElse(loop(tail))
        case Nil          => None
      }
    loop(scopes)
  }

  def set(expr: Expr, tpe: Type): TypeEnv =
    TypeEnv(scopes.head.set(expr, tpe) :: scopes.tail)

  def gen: (Type.Var, TypeEnv) = {
    val (variable, head) = scopes.head.gen
    (variable, TypeEnv(head :: scopes.tail))
  }

  def push: TypeEnv =
    TypeEnv(TypeScope.create :: scopes)

  def pop: TypeEnv =
    TypeEnv(scopes.tail)
}

object TypeEnv {
  def create: TypeEnv =
    create(TypeScope.create)

  def create(scope: TypeScope): TypeEnv =
    TypeEnv(List(scope))

  private var nextVar = 0
  def genVar: Type.Var = {
    nextVar = nextVar + 1
    Type.Var(nextVar)
  }
}
