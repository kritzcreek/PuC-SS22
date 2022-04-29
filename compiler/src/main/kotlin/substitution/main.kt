package substitution

sealed class Expr {
  // Lambda Calculus
  data class Var(val name: String) : Expr()
  data class Lambda(val binder: String, val body: Expr) : Expr()
  data class App(val func: Expr, val arg: Expr) : Expr()

  data class If(val condition: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr()

  data class IntLiteral(val num: Int) : Expr()
  data class BoolLiteral(val bool: Boolean) : Expr()
  data class Addition(val left: Expr, val right: Expr) : Expr()
  data class Equality(val left: Expr, val right: Expr) : Expr()
}

fun substitute(binder: String, replacement: Expr, expr: Expr): Expr {
  return when (expr) {
    is Expr.IntLiteral, is Expr.BoolLiteral -> expr
    is Expr.App -> Expr.App(
      substitute(binder, replacement, expr.func),
      substitute(binder, replacement, expr.arg)
    )
    is Expr.If -> Expr.If(
      substitute(binder, replacement, expr.condition),
      substitute(binder, replacement, expr.thenBranch),
      substitute(binder, replacement, expr.elseBranch)
    )
    is Expr.Lambda -> {
      // Shadowing!
      if (expr.binder == binder) {
        expr
      } else {
        Expr.Lambda(expr.binder, substitute(binder, replacement, expr.body))
      }
    }
    is Expr.Var -> if (expr.name == binder) {
      replacement
    } else {
      expr
    }
    is Expr.Addition -> Expr.Addition(
      substitute(binder, replacement, expr.left),
      substitute(binder, replacement, expr.right)
    )
    is Expr.Equality -> Expr.Equality(
      substitute(binder, replacement, expr.left),
      substitute(binder, replacement, expr.right)
    )
  }
}

fun eval(expr: Expr): Expr {
  return when (expr) {
    is Expr.App -> {
      // Beta Reduktion
      val evaledFunc = eval(expr.func)
      if (evaledFunc is Expr.Lambda) {
        val evaledArg = eval(expr.arg)
        eval(substitute(evaledFunc.binder, evaledArg, evaledFunc.body))
      } else {
        Expr.App(evaledFunc, expr.arg)
      }
    }
    is Expr.Lambda, is Expr.IntLiteral, is Expr.BoolLiteral, is Expr.Var -> expr
    is Expr.Addition -> {
      val left = eval(expr.left)
      val right = eval(expr.right)
      if (left is Expr.IntLiteral && right is Expr.IntLiteral) {
        Expr.IntLiteral(left.num + right.num)
      } else {
        Expr.Addition(left, right)
      }
    }
    is Expr.Equality -> {
      val left = eval(expr.left)
      val right = eval(expr.right)
      if (left is Expr.IntLiteral && right is Expr.IntLiteral) {
        Expr.BoolLiteral(left.num == right.num)
      } else {
        Expr.Equality(left, right)
      }
    }
    is Expr.If -> {
      val condition = eval(expr.condition)
      if (condition is Expr.BoolLiteral) {
        if (condition.bool) {
          eval(expr.thenBranch)
        } else {
          eval(expr.elseBranch)
        }
      } else {
        Expr.If(condition, expr.thenBranch, expr.elseBranch)
      }
    }
  }
}

val twice = Expr.Lambda(
  "f",
  Expr.Lambda(
    "x",
    Expr.App(Expr.Var("f"), Expr.App(Expr.Var("f"), Expr.Var("x")))
  )
)

val addOne = Expr.Lambda(
  "x",
  Expr.Addition(Expr.Var("x"), Expr.IntLiteral(1))
)

val omega = Expr.Lambda("x", Expr.App(Expr.Var("x"), Expr.Var("x")))

val x = Expr.Var("x")
val v = Expr.Var("v")
val f = Expr.Var("f")
val y = Expr.Var("y")

val innerZ = Expr.Lambda("v", Expr.App(Expr.App(x, x), v))
val innerZ1 = Expr.Lambda("x", Expr.App(f, innerZ))
val z = Expr.Lambda("f", Expr.App(innerZ1, innerZ1))


// sumAll(4) = 1 + 2 + 3 + 4
// sumAll(6) = 1 + 2 + 3 + 4 + 5 + 6

// sumAll(n) = if n == 0 then 0 else n + sumAll(n - 1)
// \self => \x => if x == 0 then 0 else x + self (x - 1)

fun main() {
  val sumAll = Expr.Lambda("self", Expr.Lambda("x",
    Expr.If(
      Expr.Equality(x, Expr.IntLiteral(0)),
      Expr.IntLiteral(0),
      Expr.Addition(
        x,
        Expr.App(Expr.Var("self"), Expr.Addition(x, Expr.IntLiteral(-1))))
    )))

  val expr = Expr.App(Expr.App(z, sumAll), Expr.IntLiteral(1000))

  println(eval(expr))
}