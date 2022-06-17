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
  data class StringLiteral(val string: String) : Expr()
}

enum class Operator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Equality,
  Concat
}

typealias Env = PersistentMap<String, Value>

sealed class Value {
  data class Int(val num: kotlin.Int) : Value()
  data class Bool(val bool: Boolean) : Value()
  data class String(val string: kotlin.String) : Value()
  data class Closure(var env: Env, val binder: kotlin.String, val body: Expr) : Value()
}

fun eval(env: Env, expr: Expr): Value {
  return when (expr) {
    is Expr.IntLiteral -> Value.Int(expr.num)
    is Expr.BoolLiteral -> Value.Bool(expr.bool)
    is Expr.StringLiteral -> Value.String(expr.string)
    is Expr.Binary -> {
      val left = eval(env, expr.left)
      val right = eval(env, expr.right)
      return when (expr.op) {
        Operator.Equality -> if (left is Value.Int && right is Value.Int) {
          Value.Bool(left.num == right.num)
        } else if (left is Value.Bool && right is Value.Bool) {
          Value.Bool(left.bool == right.bool)
        } else if (left is Value.String && right is Value.String) {
          Value.Bool(left.string == right.string)
        } else {
          throw Error("Comparing incompatible values: $left and $right")
        }
        Operator.Concat -> if (left is Value.String && right is Value.String) {
          Value.String(left.string + right.string)
        } else {
          throw Error("Can't concatenate non-string values: $left and $right")
        }
        else -> numericBinary(left, right, nameForOp(expr.op)) { x, y -> applyOp(expr.op, x, y) }
      }

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
    is Expr.Var ->
      when (expr.name) {
        "#firstChar" -> {
          val s = env["x"]!! as Value.String
          Value.String(s.string.take(1))
        }
        "#remainingChars" -> {
          val s = env["x"]!! as Value.String
          Value.String(s.string.drop(1))
        }
        "#charCode" -> {
          val s = env["x"]!! as Value.String
          Value.Int(s.string[0].code)
        }
        "#codeChar" -> {
          val x = env["x"]!! as Value.Int
          Value.String(x.num.toChar().toString())
        }
        else -> env.get(expr.name) ?: throw Exception("Unbound variable ${expr.name}")
      }
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
    else -> throw Error("Can't concat ints")
  }
}

fun nameForOp(op: Operator): String {
  return when (op) {
    Operator.Add -> "add"
    Operator.Subtract -> "subtract"
    Operator.Multiply -> "multiply"
    Operator.Divide -> "divide"
    Operator.Equality -> "compare"
    Operator.Concat -> "concat"
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
val initialEnv: Env = persistentHashMapOf(
  "firstChar" to Value.Closure(emptyEnv, "x",
    Expr.Var("#firstChar")
  ),
  "remainingChars" to Value.Closure(emptyEnv, "x",
    Expr.Var("#remainingChars")
  ),
  "charCode" to Value.Closure(emptyEnv, "x",
    Expr.Var("#charCode")
  ),
  "codeChar" to Value.Closure(emptyEnv, "x",
    Expr.Var("#codeChar")
  )
)
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

fun testInput(input: String) {
  val expr = Parser(Lexer(input)).parseExpression()
  val ty = infer(initialContext, expr)

  println("${eval(initialEnv, expr) } : ${prettyPoly(generalize(initialContext, applySolution(ty)))}")
}

fun main() {
  testInput("""
    let hello = "Hello" in
    let world = "World" in
    let join = \s1 => \s2 => s1 # " " # s2 in
    let shout = \s => s # "!" in
    let twice = \f => \x => f (f x) in
    twice (twice shout) (join hello world)
  """.trimIndent())


}