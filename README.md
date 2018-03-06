# Atlas

A tiny embedded scripting language implemented in Scala.

Copyright 2018 Dave Gurnell. Licensed [Apache 2](https://www.apache.org/licenses/LICENSE-2.0).

[![Build Status](https://travis-ci.org/davegurnell/atlas.svg?branch=develop)](https://travis-ci.org/davegurnell/atlas)
[![Coverage status](https://img.shields.io/codecov/c/github/davegurnell/atlas/develop.svg)](https://codecov.io/github/davegurnell/atlas)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.underscore/atlas_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.underscore/atlas_2.11)

## Why?

Atlas is a super-simple scripting language
created for use in [Cartographer](https://cartographer.io)
to allow us to define fragments of logic over user-defined data types.
It has the following features:

- simple, concise, expression-oriented syntax;
- Scheme-like functional semantics;
- parser and interpreter written in Scala;
- serializable in compiled and uncompiled forms;
- support for "native functions" written in Scala.

Atlas is currently a work-in-progress.

## Show me some examples!

Factorials are the "hello world" of scripting languages, right?

```ruby
let factorial = n ->
  if n <= 1
  then 1
  else n * factorial(n - 1)

factorial(10)
```

Function bodies are lazily bound
allowing `letrec`-style recursive references:

```ruby
let even = n ->
  if n == 0 then true else odd(n - 1)

let odd = n ->
  if n == 0 then false else even(n - 1)

even(10)
```

## Quick language reference

Basic literals are Javascript-like:

```
'foo'      # single-quoted string
"foo"      # double-quoted string
1          # integer
1.2        # double
true       # boolean
null       # null

[a, b]     # array
{a:1, b:2} # object
```

There are a fixed set of built-in
prefix and infix operators.
In order of decreasing precedence these are:

```
!a         # boolean not
-a         # negation
+a         # erm... non-negation

a*b        # multiplication
a/b        # floating point division

a+b        # addition
a-b        # addition

a < b      # comparisons
a > b      # (integer, double, string, or boolean)
a <= b     #
a >= b     #

a == b     # value equality and
a != b     # function reference equality

a && b     # boolean and

a || b     # boolean or
```

Function literals are written with the `->` symbol.
Parentheses are optional if there is only one argument:

```
(a, b) -> a + b
n -> n + 1
```

In addition to literals, variable references,
and infix and prefix operators,
there are several types of expression.

Function applications look like Javascript:

```
max(1, 2)
```

as do field references:

```
foo.bar.baz
```

Conditionals are introduced with
the `if`, `then`, and `else` keywords.
The `else` clause is mandatory.
The result is the value
of the expression in the relevant arm:

```
if expr then expr else expr
```

Blocks introduce scopes
and allow the definition of intermediate variables.
The result is the value of the final expression:

```
do
  stmt
  stmt
  expr
end
```

Statements are expressions (evaluated for their side-effects)
or declarations, introduced with the `let` keyword:

```
let add = (a, b) -> a + b
let ten = add(3, 7)
```

Function bodies can refer to earlier or later bindings
in the block where they are defined,
allowing mutually recursive definitions:

```
let even = n ->
  if n == 0 then true else odd(n - 1)

let odd  = n ->
  if n == 0 then false else even(n - 1)

even(10)
```

Comments are written with the `#` symbol
and run to the end of the line:

```
# Calculate a factorial:
let fact = n ->
  if n == 1
  then 1
  else n * fact(n - 1)
```

Complete programs have
the same semantics as blocks
but are written without the `do` and `end` keywords.
If the program ends with a statement,
an implicit `null` expression is added to the end:

```
let fib = n ->
  if n <= 2
  then 1
  else fib(n - 1) + fib(n - 2)

fib(10)
```

## Interaction with Scala

There are two string interpolators
for defining code fragments:
`expr` for expressions
and `prog` for complete programs:

```scala
import atlas._
import atlas.syntax._

val anExpression: Ast.Expr =
  expr"""
  1 + 2 + 3
  """

val aProgram: Ast.Expr =
  prog"""
  let fib = n ->
    if n <= 2
    then 1
    else fib(n - 1) + fib(n - 2)

  fib(10)
  """
```

Syntax errors raised by the macros
result in a Scala compilation error.

You can alternatively use
the `Parser.expr` or `Parser.prog` methods
to parse a regular Scala string:
Syntax errors using the parser result in an `Either`:

```scala
val anotherExpression: Either[Parser.Error, Ast.Expr] =
  Parser.expr("1 + 2 + 3")
```

Expressions (and, by extension, programs)
can be evaluated using the `Eval.apply` method.
Runtime errors are captured in an `Either`:

```scala
Eval(anExpression) // => Right(IntValue(6))

Eval(aProgram)     // => Right(IntValue(55))
```

You can optionally pass an `Env` object to `Eval.apply`
specifying an initial environment:

```scala
val program = prog"a + b"
val env = Env.create
  .set("a", 10)
  .set("b", 32)
Eval(program, env) // => Right(IntValue(42))
```

Although `Eval` can internally mutate environments
to enable mutually recursive function bodies,
any environment you pass to `Eval.apply`
should be returned unharmed.

You can implement "native functions" in Scala:

```scala
val program = prog"average(10, 5)"
val env = Env.create
  .set("average", NativeFunc((a: Double, b: Double) => (a + b) / 2))
Eval(program, env) // => Right(DoubleValue(7.5))
```

Conversion between Atlas and Scala values
is implemented using a pair of type classes
called `ValueEncoder` and `ValueDecoder`.
These can be used with the `as[A]` and `asValue`
extension methods from `atlas.syntax`:

```scala
123.asValue      // => IntValue(123)

IntValue.as[Int] // => Right(123)
```

## Acknowledgements

Thanks to Nic Pereira for naming the project and saving us all from "davescript" :)
