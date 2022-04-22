sealed class Expr {
  // Lambda Calculus
  data class Var(val name: String) : Expr()
  data class Lambda(val binder: String, val body: Expr) : Expr()
  data class App(val func: Expr, val arg: Expr) : Expr()

  // Zur Anschauligkeit
  data class Literal(val num: Int) : Expr()
  data class Addition(val left: Expr, val right: Expr) : Expr()
}

fun substitute(binder: String, replacement: Expr, expr: Expr): Expr {
  return when (expr) {
    is Expr.Literal -> expr
    is Expr.App -> Expr.App(
      substitute(binder, replacement, expr.func),
      substitute(binder, replacement, expr.arg)
    )
    is Expr.Lambda -> {
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
    is Expr.Lambda, is Expr.Literal, is Expr.Var -> expr
    is Expr.Addition -> {
      val left = eval(expr.left)
      val right = eval(expr.right)
      if (left is Expr.Literal && right is Expr.Literal) {
        Expr.Literal(left.num + right.num)
      } else {
        Expr.Addition(left, right)
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
  Expr.Addition(Expr.Var("x"), Expr.Literal(1))
)

val omega = Expr.Lambda("x", Expr.App(Expr.Var("x"), Expr.Var("x")))

fun main() {
  val fortytwo = Expr.App(addOne, Expr.Literal(41))

  val result = Expr.App(Expr.App(twice, addOne), Expr.Literal(15))
  val scary = Expr.App(omega, omega)
  println(eval(scary))
}