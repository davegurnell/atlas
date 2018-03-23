package atlas

import atlas.syntax._
import minitest._
import unindent._

object TypeUnifierSuite extends SimpleTestSuite {
  test("compose") {
    val subst1 = Set(v(0) --> v(1))
    val subst2 = Set(v(1) --> v(2))

    val actual   = TypeUnifier.compose(subst1, subst2).toList.sortBy(_.lhs.id)
    val expected = List(v(0) --> v(2), v(1) --> v(2))

    assertEquals(actual, expected)
  }

  test("constant") {
    assertSuccess(
      expr"true",
      List(
        v(0) --> BoolType
      ))

    assertSuccess(
      expr"1",
      List(
        v(0) --> IntType
      ))
  }

  test("cond") {
    assertSuccess(
      expr"if true then 123 else 456",
      List(
        v(0) --> IntType,
        v(1) --> BoolType,
        v(2) --> IntType,
        v(3) --> IntType
      ))
  }

  test("infix") {
    assertSuccess(
      expr"123 + 456",
      List(
        v(0) --> IntType,
        v(1) --> IntType,
        v(2) --> IntType
      ))
  }

  // test("prefix") {
  //   assertSuccess(
  //     expr"!!true",
  //     List(
  //       v(0) === FuncType(List(BoolType), BoolType),
  //       v(1) === FuncType(List(BoolType), BoolType),
  //       v(2) === BoolType,
  //       v(1) === FuncType(List(v (2)), BoolType),
  //       v(0) === FuncType(List(v (1)), BoolType)
  //     ))
  // }

  // test("cast") {
  //   assertSuccess(
  //     expr"1 : Int : Real",
  //     List(
  //       v(2) === IntType,
  //       v(1) === v(2),
  //       v(1) === IntType,
  //       v(0) === v(1),
  //       v(0) === DblType
  //     ))
  // }

  // test("block") {
  //   assertSuccess(
  //     expr"do 1; true end",
  //     List(
  //       v(1) === IntType,
  //       v(2) === BoolType,
  //       v(0) === v(2)
  //     ))
  // }

  // test("let / ref") {
  //   assertSuccess(
  //     expr"do let a = 1; a end",
  //     List(
  //       v(2) === IntType,
  //       v(1) === v(2),
  //       v(0) === v(1)
  //     ))

  //   assertSuccess(
  //     expr"do let a: Boolean = 1; a end",
  //     List(
  //       v(2) === IntType,
  //       v(1) === BoolType,
  //       v(1) === v(2),
  //       v(0) === v(1)
  //     ))
  // }

  // test("func / apply") {
  //   assertSuccess(
  //     expr"(a, b) -> a(b)",
  //     List(
  //       v(3) === FuncType(List(v(2)), v(1)),
  //       v(0) === v(3)
  //     ))

  //   assertSuccess(
  //     expr"(a: Int -> String, b: Real): Boolean -> a(b)",
  //     List(
  //       v(3) === FuncType(List(v(2)), v(1)),
  //       v(1) === FuncType(List(IntType), StrType),
  //       v(2) === DblType,
  //       v(0) === BoolType,
  //       v(0) === v(3)
  //     ))
  // }

  // test("block scope") {
  //   assertSuccess(
  //     expr"""
  //     do
  //       let a = 1
  //       do
  //         let a = 'hi'
  //         a
  //       end
  //       a
  //     end
  //     """,
  //     List(
  //       v(2) === IntType,
  //       v(1) === v(2),
  //       v(5) === StrType,
  //       v(4) === v(5),
  //       v(3) === v(4),
  //       v(0) === v(1)
  //     ))
  // }

  // test("func scope") {
  //   assertSuccess(
  //     expr"""
  //     do
  //       let apply = (a, b) -> a(b)
  //       apply(1, 2)
  //     end
  //     """,
  //     List(
  //       v(5) === FuncType(List(v(4)), v(3)),
  //       v(2) === v(5),
  //       v(1) === v(2),
  //       v(7) === IntType,
  //       v(8) === IntType,
  //       v(6) === FuncType(List(v(7), v(8)), v(1)),
  //       v(0) === v(6)
  //     ))
  // }

  // test("mutual recursion") {
  //   assertSuccess(
  //     expr"""
  //     do
  //       let a = n -> b(n)
  //       let b = n -> a(n)
  //       a(1)
  //     end
  //     """,
  //     List(
  //       v(5)  === FuncType(List(v(4)), v(2)),
  //       v(3)  === v(5),
  //       v(1)  === v(3),
  //       v(8)  === FuncType(List(v(7)), v(1)),
  //       v(6)  === v(8),
  //       v(2)  === v(6),
  //       v(10) === IntType,
  //       v(9)  === FuncType(List(v(10)), v(1)),
  //       v(0)  === v(9)
  //     ))
  // }

  def assertSuccess(expr: Expr, expected: List[Substitution], env: Env = Env.create): Unit = {
    val either = for {
      texpr       <- TypeAnnotator(expr)
      constraints <- TypeGenerator(texpr)
      actual      <- TypeUnifier(constraints)
    } yield (texpr, constraints, actual)

    either match {
      case Right((texpr, constraints, actual)) =>
        assert(
          actual == expected,
          i"""
          Incorrect results from type checking:
          expr = $expr
          texpr = $texpr
          constraints =
            ${constraints.mkString("\n  ")}
          actual =
            ${actual.mkString("\n  ")}
          expected =
            ${expected.mkString("\n  ")}
          """)

      case Left(error) =>
        fail(
          i"""
          Expected type checking to succeed, but it failed:
          $error
          """)

    }
  }

  def assertFailure(expr: Expr, expected: TypeError, env: Env = Env.create): Unit = {
    val either = for {
      texpr       <- TypeAnnotator(expr)
      constraints <- TypeGenerator(texpr)
      substs      <- TypeUnifier(constraints)
    } yield (texpr, constraints, substs)

    either match {
      case Right((texpr, constraints, substs)) =>
        fail(
          i"""
          Expected type checking to fail, but it succeeded:
          expr = $expr
          texpr = $texpr
          constraints =
            ${constraints.mkString("\n  ")}
          substs =
            ${substs.mkString("\n  ")}
          """)

      case Left(actual) =>
        assert(
          actual == expected,
          i"""
          Type checking failed with an unexpected error:
          expr = $expr
          actual = $actual
          expected = $expected
          """)

    }
  }

  def v(id: Int): TypeVar =
    TypeVar(id)
}
