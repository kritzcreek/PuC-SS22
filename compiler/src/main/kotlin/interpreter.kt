import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf

sealed class Expr {
  data class Var(val name: String) : Expr()
  data class Lambda(val binder: String, val tyBinder: MonoType?, val body: Expr) : Expr()
  data class App(val func: Expr, val arg: Expr) : Expr()
  data class If(val condition: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr()
  data class Binary(val left: Expr, val op: Operator, val right: Expr) : Expr()
  data class Let(val recursive: Boolean, val binder: String, val expr: Expr, val body: Expr) : Expr()

  data class IntLiteral(val num: Int) : Expr()
  data class BoolLiteral(val bool: Boolean) : Expr()
}

enum class Operator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Equality
}

typealias Env = PersistentMap<String, Value>

sealed class Value {
  data class Int(val num: kotlin.Int) : Value()
  data class Bool(val bool: Boolean) : Value()
  data class Closure(var env: Env, val binder: String, val body: Expr) : Value()
}

fun eval(env: Env, expr: Expr): Value {
  return when (expr) {
    is Expr.IntLiteral -> Value.Int(expr.num)
    is Expr.BoolLiteral -> Value.Bool(expr.bool)
    is Expr.Binary -> {
      val left = eval(env, expr.left)
      val right = eval(env, expr.right)
      numericBinary(left, right, nameForOp(expr.op)) { x, y -> applyOp(expr.op, x, y) }
    }
    is Expr.If -> {
      val condition = eval(env, expr.condition)
      if (condition !is Value.Bool) {
        throw Exception("Expected a boolean condition, but got $condition")
      }
      return if (condition.bool) {
        eval(env, expr.thenBranch)
      } else {
        eval(env, expr.elseBranch)
      }
    }
    is Expr.Let -> {
      val evaledExpr = eval(env, expr.expr)
      if (expr.recursive && evaledExpr is Value.Closure) {
        evaledExpr.env = evaledExpr.env.put(expr.binder, evaledExpr)
      }
      val extendedEnv = env.put(expr.binder, evaledExpr)
      eval(extendedEnv, expr.body)
    }
    is Expr.Lambda -> Value.Closure(env, expr.binder, expr.body)
    is Expr.Var -> env.get(expr.name) ?: throw Exception("Unbound variable ${expr.name}")
    is Expr.App -> {
      val func = eval(env, expr.func)
      if (func !is Value.Closure) {
        throw Exception("$func is not a function")
      } else {
        val arg = eval(env, expr.arg)
        val newEnv = func.env.put(func.binder, arg)
        eval(newEnv, func.body)
      }
    }
  }
}

fun applyOp(op: Operator, x: Int, y: Int): Value {
  return when (op) {
    Operator.Add -> Value.Int(x + y)
    Operator.Subtract -> Value.Int(x - y)
    Operator.Multiply -> Value.Int(x * y)
    Operator.Divide -> Value.Int(x / y)
    Operator.Equality -> Value.Bool(x == y)
  }
}

fun nameForOp(op: Operator): String {
  return when (op) {
    Operator.Add -> "add"
    Operator.Subtract -> "subtract"
    Operator.Multiply -> "multiply"
    Operator.Divide -> "divide"
    Operator.Equality -> "compare"
  }
}

fun numericBinary(left: Value, right: Value, operation: String, combine: (Int, Int) -> Value): Value {
  if (left is Value.Int && right is Value.Int) {
    return combine(left.num, right.num)
  } else {
    throw (Exception("Can't $operation non-numbers, $left, $right"))
  }
}

val emptyEnv: Env = persistentHashMapOf()
//val x = Expr.Var("x")
//val y = Expr.Var("y")
//val v = Expr.Var("v")
//val f = Expr.Var("f")
//
//val innerZ = Expr.Lambda("v", Expr.App(Expr.App(x, x), v))
//val innerZ1 = Expr.Lambda("x", Expr.App(f, innerZ))
//val z = Expr.Lambda("f", Expr.App(innerZ1, innerZ1))

// Hausaufgabe:
// Fibonacci Funktion implementieren
// fib(0) = 1
// fib(1) = 1
// fib(x) = fib (x - 1) + fib (x - 2)

tailrec fun sum(acc: Int, x: Int): Int {
  return if (x == 0) {
    acc
  } else {
    sum(acc + x, x - 1)
  }
}

fun main() {
  // val sum100000 = sum(0, 100000)

  val input = """
    let rec sum = \x => if x == 0 then 0 else x + sum (x - 1) in
    let rec fib = \x =>
      if x == 0 then 0
      else if x == 1 then 1
      else fib (x - 1) + fib (x - 2) in
    sum 10000
  """.trimIndent()

//  val input = """
//    let flip = \f => \x => \y => f y x in
//    flip
//  """.trimIndent()

  val expr = Parser(Lexer(input)).parseExpression()
  val ty = infer(emptyContext, expr)
  println("${eval(emptyEnv, expr) }: ${prettyPoly(generalize(emptyContext, applySolution(ty)))}")

  // sumAll(0) = 0
  // sumAll(x) = x + sumAll(x-1)
//  val sumAll = Expr.Lambda(
//    "self", Expr.Lambda(
//      "x",
//      Expr.If(
//        Expr.Binary(x, Operator.Equality, Expr.IntLiteral(0)),
//        Expr.IntLiteral(0),
//        Expr.Binary(
//          x,
//          Operator.Add,
//          Expr.App(Expr.Var("self"), Expr.Binary(x, Operator.Add, Expr.IntLiteral(-1)))
//        )
//      )
//    )
//  )

//  val add = Expr.Lambda("x", Expr.Lambda("y", Expr.Binary(x, Operator.Add, y)))

//  val result = eval(
//    emptyEnv,
//    Expr.App(Expr.App(z, sumAll), Expr.IntLiteral(500))
//  )
//  println("$result")
}