import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf

sealed class Type {
  object IntTy : Type()
  object BoolTy : Type()
  data class FunType(val arg: Type, val returnTy: Type) : Type()
}

fun prettyTy(ty: Type, nested: Boolean = false): String {
  return when (ty) {
    Type.BoolTy -> "Bool"
    is Type.FunType -> {
      val prettyArg = prettyTy(ty.arg, true)
      val prettyReturn = prettyTy(ty.returnTy)
      if (nested) {
        "($prettyArg -> $prettyReturn)"
      } else {
        "$prettyArg -> $prettyReturn"
      }
    }
    Type.IntTy -> "Int"
  }
}

typealias Context = PersistentMap<String, Type>

val emptyContext: Context = persistentHashMapOf()

fun infer(ctx: Context, expr: Expr): Type {
  return when (expr) {
    is Expr.IntLiteral -> Type.IntTy
    is Expr.BoolLiteral -> Type.BoolTy
    is Expr.If -> {
      val tyCond = infer(ctx, expr.condition)
      val tyThen = infer(ctx, expr.thenBranch)
      val tyElse = infer(ctx, expr.elseBranch)
      if (tyCond != Type.BoolTy) throw Exception("${expr.condition} does not have type Bool")
      if (tyThen != tyElse) throw Exception("Branches in if have mismatching types")
      tyThen
    }
    is Expr.Var -> {
      ctx.get(expr.name) ?: throw Exception("Unknown variable ${expr.name}")
    }
    is Expr.Let -> {
      val tyExpr = infer(ctx, expr.expr)
      val tyBody = infer(ctx.put(expr.binder, tyExpr), expr.body)
      tyBody
    }
    is Expr.Binary -> {
      val tyLeft = infer(ctx, expr.left)
      val tyRight = infer(ctx, expr.right)
      if (tyLeft != Type.IntTy) throw Exception("${expr.left} is not an Int")
      if (tyRight != Type.IntTy) throw Exception("${expr.right} is not an Int")
      when (expr.op) {
        Operator.Add,
        Operator.Subtract,
        Operator.Multiply,
        Operator.Divide ->
         Type.IntTy
        Operator.Equality -> Type.BoolTy
      }
    }
    is Expr.App -> {
      val tyFunc = infer(ctx, expr.func)
      val tyArg = infer(ctx, expr.arg)
      when (tyFunc) {
        is Type.FunType -> {
          if (tyArg != tyFunc.arg) throw Exception("Expected a value of type ${tyFunc.arg} but got ${expr.arg}")
          tyFunc.returnTy
        }
        else -> throw Exception("$tyFunc is not a function type")
      }
    }
    is Expr.Lambda -> {
      // TODO: Grammatik anpassen, User muss typ fuer binder angeben
      val tyBinder = Type.BoolTy
      val tyBody = infer(ctx.put(expr.binder, tyBinder), expr.body)
      Type.FunType(tyBinder, tyBody)
    }
  }
}

fun testInfer(program: String) {
  val expr = Parser(Lexer(program)).parseExpression()
  val ty = infer(emptyContext, expr)
  println("$program : ${prettyTy(ty)}")
}

fun main() {
  testInfer("10")
  testInfer("true")
  testInfer("if true then 10 else 20")
  testInfer(
    """
    let x = 20 in
    let y = true in
    if y then x else 10
  """.trimIndent()
  )
  testInfer("1 + 2")
  testInfer("1 + 2 == 3 * 4")
  testInfer("""
    let not = \x => if x then false else true in
    let boolEq = \x => \y => if x then y else not y in
    if boolEq (2 == 3) (3 == 4) then 10 else 20
  """.trimIndent())

}


