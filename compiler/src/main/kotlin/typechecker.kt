import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.PersistentSet
import kotlinx.collections.immutable.persistentHashMapOf
import kotlinx.collections.immutable.persistentSetOf

sealed class Type {
  object IntTy : Type()
  object BoolTy : Type()
  data class FunType(val arg: Type, val returnTy: Type) : Type() {
    override fun toString(): String {
      return super.toString()
    }
  }
  data class Unknown(val u: Int) : Type() {
    override fun toString(): String {
      return super.toString()
    }
  }

  override fun toString(): String {
    return prettyTy(this, false)
  }

  fun unknowns(): PersistentSet<Int> {
    return when(this){
      BoolTy, IntTy -> persistentSetOf()
      is FunType -> this.arg.unknowns().addAll(this.returnTy.unknowns())
      is Unknown -> persistentSetOf(this.u)
    }
  }
}

fun prettyTy(ty: Type, nested: Boolean = false): String {
  return when (ty) {
    Type.BoolTy -> "Bool"
    is Type.Unknown -> "$${ty.u}"
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
      shouldEqual(tyCond, Type.BoolTy)
      // if (tyCond != Type.BoolTy) throw Exception("${expr.condition} does not have type Bool")
      shouldEqual(tyThen, tyElse)
      // if (tyThen != tyElse) throw Exception("Branches in if have mismatching types")
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
      shouldEqual(tyLeft, Type.IntTy)
      shouldEqual(tyRight, Type.IntTy)
//      if (tyLeft != Type.IntTy) throw Exception("${expr.left} is not an Int, but a $tyLeft instead")
//      if (tyRight != Type.IntTy) throw Exception("${expr.right} is not an Int, but a $tyRight instead")
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
      val tyRes = freshUnknown()
      shouldEqual(tyFunc, Type.FunType(tyArg, tyRes))
      return tyRes
    }
    is Expr.Lambda -> {
      val tyBinder = expr.tyBinder ?: freshUnknown()
      val tyBody = infer(ctx.put(expr.binder, tyBinder), expr.body)
      Type.FunType(tyBinder, tyBody)
    }
  }
}

fun unify(t1: Type, t2: Type) {
  val ty1 = applySolution(t1)
  val ty2 = applySolution(t2)
  if (ty1 == ty2) return
  else if (ty1 is Type.Unknown) {
    solve(ty1.u, ty2)
  }
  else if (ty2 is Type.Unknown) {
    solve(ty2.u, ty1)
  }
  else if (ty1 is Type.FunType && ty2 is Type.FunType) {
    unify(ty1.arg, ty2.arg)
    unify(ty1.returnTy, ty2.returnTy)
  } else {
    throw Error("Can't unify $ty1 with $ty2")
  }
}

var unknownSupply: Int = 0
fun freshUnknown(): Type {
  return Type.Unknown(++unknownSupply)
}

fun shouldEqual(ty1: Type, ty2: Type) {
  unify(ty1, ty2)
}

var solution: MutableMap<Int, Type> = mutableMapOf()
fun solve(u: Int, ty: Type) {
  // Occurs check:
  if (ty.unknowns().contains(u)) {
    throw Exception("Occurs check failed for: $$u and $ty")
  }
  solution[u] = ty
}

fun applySolution(ty: Type): Type {
  return when(ty){
    Type.BoolTy, Type.IntTy -> ty
    is Type.FunType -> Type.FunType(applySolution(ty.arg), applySolution(ty.returnTy))
    is Type.Unknown -> solution[ty.u] ?: ty
  }
}

fun testInfer(program: String) {
  unknownSupply = 0
  solution = mutableMapOf()
  val expr = Parser(Lexer(program)).parseExpression()
  val ty = applySolution(infer(emptyContext, expr))
  println("$program : ${prettyTy(ty)}")
//  println("Solution:")
//  solution.forEach { (t1, t2) -> println("$$t1 := ${prettyTy(t2)}")}
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
    let not = \x : Bool => if x then false else true in
    let boolEq = \x : Bool => \y : Bool => if x then y else not y in
    if boolEq (2 == 3) (3 == 4) then 10 else 20
  """.trimIndent())
  testInfer("""
    (\x => x) true
  """.trimIndent())
}


