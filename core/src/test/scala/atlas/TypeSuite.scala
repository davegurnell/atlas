package atlas

import atlas.syntax._
import minitest._

object TypeSuite extends SimpleTestSuite {
  test("isAssignable") {
    assert(Type.isAssignable(IntType, IntType))

    assert(Type.isAssignable(IntType | StrType, IntType))
    assert(Type.isAssignable(IntType | StrType, StrType))

    assert(!Type.isAssignable(IntType, IntType | StrType))
    assert(!Type.isAssignable(StrType, IntType | StrType))
  }
}