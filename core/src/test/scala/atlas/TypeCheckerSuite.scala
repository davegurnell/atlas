package atlas

import atlas.syntax._
import cats.{Eval, MonadError}
import cats.data.EitherT
import cats.implicits._
import minitest._

object TypeCheckerSuite extends SimpleTestSuite {
  test("lit") {
    assertSuccess(expr"true",   BoolType)
    assertSuccess(expr"123",    IntType)
    assertSuccess(expr"123.0",  DblType)
    assertSuccess(expr"'Dave'", StrType)
    assertSuccess(expr"null",   NullType)
  }

  test("ref") {
    assertSuccess(prog"""let x = true   ; x""", BoolType)
    assertSuccess(prog"""let x = 123    ; x""", IntType)
    assertSuccess(prog"""let x = 123.0  ; x""", DblType)
    assertSuccess(prog"""let x = 'Dave' ; x""", StrType)
    assertSuccess(prog"""let x = null   ; x""", NullType)
  }

  test("cond") {
    assertSuccess(prog"""if true then 123 else 456""",    IntType)
    assertSuccess(prog"""if true then 123 else 'Dave'""", IntType | StrType)
    assertSuccess(prog"""if true then 123 else null""",   IntType.?)
    assertFailure(prog"""if 123  then 123 else null""",   TypeError.typeMismatch(BoolType, IntType))
  }

  test("cast") {
    assertSuccess(expr"""123 : (Int | String)""", IntType | StrType)
    assertFailure(expr"""(if true then 123 else 'Dave') : Int""", TypeError.typeMismatch(IntType, IntType | StrType))
  }

  test("array") {
    assertSuccess(expr"""[123, 234, 345]""",     ArrType(IntType))
    assertSuccess(expr"""[123, true, 'Dave']""", ArrType(IntType | BoolType | StrType))
    assertSuccess(expr"""[]""",                  ArrType(Type.emptyUnion))
  }

  def assertSuccess[A](expr: Expr, expected: Type): Unit =
    assertEquals(TypeAnnotator(expr).flatMap(TypeChecker.check(_)), Right(expected))

  def assertFailure(expr: Expr, expected: TypeError): Unit =
    assertEquals(TypeAnnotator(expr).flatMap(TypeChecker.check(_)), Left(expected))
}
