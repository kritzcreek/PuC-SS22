import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf

sealed class Expr {
  data class Var(val name: String) : Expr()
  data class Lambda(val binder: String, val body: Expr) : Expr()
  data class App(val func: Expr, val arg: Expr) : Expr()
  data class If(val condition: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr()

  data class IntLiteral(val num: Int) : Expr()
  data class BoolLiteral(val bool: Boolean) : Expr()
  data class Addition(val left: Expr, val right: Expr) : Expr()
  data class Equality(val left: Expr, val right: Expr) : Expr()
}

typealias Env = PersistentMap<String, Value>

sealed class Value {
  data class Int(val num: kotlin.Int) : Value()
  data class Bool(val bool: Boolean) : Value()
  data class Closure(val env: Env, val binder: String, val body: Expr) : Value()
}

fun eval(env: Env, expr: Expr): Value {
  return when (expr) {
    is Expr.IntLiteral -> Value.Int(expr.num)
    is Expr.BoolLiteral -> Value.Bool(expr.bool)
    is Expr.Addition -> {
      val left = eval(env, expr.left)
      val right = eval(env, expr.right)
      numericBinary(left, right, "add") { x, y -> Value.Int(x + y) }
    }
    is Expr.Equality -> {
      val left = eval(env, expr.left)
      val right = eval(env, expr.right)
      numericBinary(left, right, "compare") { x, y -> Value.Bool(x == y) }
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

fun numericBinary(left: Value, right: Value, operation: String, combine: (Int, Int) -> Value): Value {
  if (left is Value.Int && right is Value.Int) {
    return combine(left.num, right.num)
  } else {
    throw (Exception("Can't $operation non-numbers, $left, $right"))
  }
}

val emptyEnv: Env = persistentHashMapOf()
val x = Expr.Var("x")
val y = Expr.Var("y")
val v = Expr.Var("v")
val f = Expr.Var("f")

val innerZ = Expr.Lambda("v", Expr.App(Expr.App(x, x), v))
val innerZ1 = Expr.Lambda("x", Expr.App(f, innerZ))
val z = Expr.Lambda("f", Expr.App(innerZ1, innerZ1))

// Hausaufgabe:
// Fibonacci Funktion implementieren
// fib(0) = 1
// fib(1) = 1
// fib(x) = fib (x - 1) + fib (x - 2)

fun main() {
  // sumAll(0) = 0
  // sumAll(x) = x + sumAll(x-1)
  val sumAll = Expr.Lambda("self", Expr.Lambda("x",
    Expr.If(
      Expr.Equality(x, Expr.IntLiteral(0)),
      Expr.IntLiteral(0),
      Expr.Addition(
        x,
        Expr.App(Expr.Var("self"), Expr.Addition(x, Expr.IntLiteral(-1))))
    )))

  val add = Expr.Lambda("x", Expr.Lambda("y", Expr.Addition(x, y)))

  val result = eval(emptyEnv,
    Expr.App(Expr.App(z, sumAll), Expr.IntLiteral(500))
  )
  println("$result")
}