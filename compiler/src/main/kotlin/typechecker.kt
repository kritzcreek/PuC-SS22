import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.PersistentSet
import kotlinx.collections.immutable.persistentHashMapOf
import kotlinx.collections.immutable.persistentSetOf

sealed class MonoType {
  object IntTy : MonoType()
  object BoolTy : MonoType()
  object StringTy : MonoType()
  data class FunType(val arg: MonoType, val returnTy: MonoType) : MonoType() {
    override fun toString(): String {
      return super.toString()
    }
  }

  data class Unknown(val u: Int) : MonoType() {
    override fun toString(): String {
      return super.toString()
    }
  }

  data class Var(val v: String) : MonoType() {
    override fun toString(): String {
      return v
    }
  }

  override fun toString(): String {
    return prettyTy(this, false)
  }

  fun unknowns(): PersistentSet<Int> {
    return when (this) {
      BoolTy, IntTy, StringTy, is Var -> persistentSetOf()
      is FunType -> this.arg.unknowns().addAll(this.returnTy.unknowns())
      is Unknown -> persistentSetOf(this.u)
    }
  }

  fun substitute(name: String, replacement: MonoType): MonoType {
    return when (this) {
      BoolTy, is Unknown, IntTy, StringTy -> this
      is FunType -> FunType(arg.substitute(name, replacement), returnTy.substitute(name, replacement))
      is Var -> if (this.v == name) {
        replacement
      } else {
        this
      }
    }
  }
}

data class PolyType(val vars: List<String>, val mono: MonoType) {
  companion object {
    fun fromMono(ty: MonoType): PolyType {
      return PolyType(emptyList(), ty)
    }
  }
}

fun instantiate(poly: PolyType): MonoType {
  return poly.vars.fold(poly.mono) { ty, v -> ty.substitute(v, freshUnknown()) }
}

fun generalize(ctx: Context, mono: MonoType): PolyType {
  val unknownsInContext: PersistentSet<Int> =
    ctx.toList().fold(persistentSetOf()) { acc, (_, v) -> acc.addAll(v.mono.unknowns()) }
  val unknowns = mono.unknowns().filterNot { u -> unknownsInContext.contains(u) }
  val map: MutableMap<Int, MonoType> = mutableMapOf()
  val zipped = unknowns.zip('a'..'z')
  zipped.forEach { (u, v) -> map[u] = MonoType.Var(v.toString()) }
  val ty = applySolution(mono, map)
  val vars = zipped.map { (_, v) -> v.toString() }
  return PolyType(vars, ty)
}

fun prettyPoly(ty: PolyType): String {
  val inner = prettyTy(ty.mono)
  return if (ty.vars.isNotEmpty()) {
    "forall ${ty.vars.joinToString(" ")}. $inner"
  } else {
    inner
  }
}

fun prettyTy(ty: MonoType, nested: Boolean = false): String {
  return when (ty) {
    MonoType.BoolTy -> "Bool"
    MonoType.IntTy -> "Int"
    MonoType.StringTy -> "String"
    is MonoType.Unknown -> "$${ty.u}"
    is MonoType.Var -> ty.v
    is MonoType.FunType -> {
      val prettyArg = prettyTy(ty.arg, true)
      val prettyReturn = prettyTy(ty.returnTy)
      if (nested) {
        "($prettyArg -> $prettyReturn)"
      } else {
        "$prettyArg -> $prettyReturn"
      }
    }
  }
}

typealias Context = PersistentMap<String, PolyType>

val initialContext: Context = persistentHashMapOf(
  "firstChar" to PolyType.fromMono(monoTy("String -> String")),
  "remainingChars" to PolyType.fromMono(monoTy("String -> String")),
  "charCode" to PolyType.fromMono(monoTy("String -> Int")),
  "codeChar" to PolyType.fromMono(monoTy("Int -> String")),
)

fun infer(ctx: Context, expr: Expr): MonoType {
  return when (expr) {
    is Expr.IntLiteral -> MonoType.IntTy
    is Expr.BoolLiteral -> MonoType.BoolTy
    is Expr.StringLiteral -> MonoType.StringTy
    is Expr.If -> {
      val tyCond = infer(ctx, expr.condition)
      val tyThen = infer(ctx, expr.thenBranch)
      val tyElse = infer(ctx, expr.elseBranch)
      shouldEqual(tyCond, MonoType.BoolTy, "A condition to an If did not have the type Bool")
      shouldEqual(tyThen, tyElse, "The then and else branch had mismatching types")
      tyThen
    }
    is Expr.Var -> {
      ctx[expr.name]?.let(::instantiate) ?: throw Exception("Unknown variable ${expr.name}")
    }
    is Expr.Let -> {
      val tyExpr = freshUnknown()
      if (expr.recursive) {
        shouldEqual(infer(ctx.put(expr.binder, PolyType.fromMono(tyExpr)), expr.expr), tyExpr, "Recursive let fail")
      } else {
        shouldEqual(infer(ctx, expr.expr), tyExpr, "Should never happen")
      }
      val tyExprPoly = generalize(ctx, applySolution(tyExpr))
      val tyBody = infer(ctx.put(expr.binder, applySolution(tyExprPoly)), expr.body)
      tyBody
    }

    is Expr.Binary -> {
      val tyLeft = infer(ctx, expr.left)
      val tyRight = infer(ctx, expr.right)

      when (expr.op) {
        Operator.Add,
        Operator.Subtract,
        Operator.Multiply,
        Operator.Divide -> {
          shouldEqual(tyLeft, MonoType.IntTy, "The left operand to ${expr.op.name} had the wrong type")
          shouldEqual(tyRight, MonoType.IntTy, "The right operand to ${expr.op.name} had the wrong type")
          MonoType.IntTy
        }
        Operator.Concat -> {
          shouldEqual(tyLeft, MonoType.StringTy, "The left operand to concat has to be String")
          shouldEqual(tyRight, MonoType.StringTy, "The right operand to concat has to be String")
          MonoType.StringTy
        }
        Operator.Equality -> {
          unify(tyLeft, tyRight)
          MonoType.BoolTy
        }
      }
    }
    is Expr.App -> {
      val tyFunc = infer(ctx, expr.func)
      val tyArg = infer(ctx, expr.arg)
      val tyRes = freshUnknown()
      shouldEqual(tyFunc, MonoType.FunType(tyArg, tyRes), "Failed to apply a function to an argument")
      return tyRes
    }
    is Expr.Lambda -> {
      val tyBinder = expr.tyBinder ?: freshUnknown()
      val tyBody = infer(ctx.put(expr.binder, PolyType.fromMono(tyBinder)), expr.body)
      MonoType.FunType(tyBinder, tyBody)
    }
  }
}

fun unify(t1: MonoType, t2: MonoType) {
  val ty1 = applySolution(t1)
  val ty2 = applySolution(t2)
  if (ty1 == ty2) return
  else if (ty1 is MonoType.Unknown) {
    solve(ty1.u, ty2)
  } else if (ty2 is MonoType.Unknown) {
    solve(ty2.u, ty1)
  } else if (ty1 is MonoType.FunType && ty2 is MonoType.FunType) {
    unify(ty1.arg, ty2.arg)
    unify(ty1.returnTy, ty2.returnTy)
  } else {
    throw Error("can't unify $ty1 with $ty2")
  }
}

var unknownSupply: Int = 0
fun freshUnknown(): MonoType {
  return MonoType.Unknown(++unknownSupply)
}

fun shouldEqual(ty1: MonoType, ty2: MonoType, msg: String) {
  try {
    unify(ty1, ty2)
  } catch (e: Error) {
    throw Error("$msg, because we ${e.message}")
  }
}

var solution: MutableMap<Int, MonoType> = mutableMapOf()
fun solve(u: Int, ty: MonoType) {
  // Occurs check:
  if (ty.unknowns().contains(u)) {
    throw Exception("Occurs check failed for: $$u and $ty")
  }
  solution[u] = ty
}

fun applySolution(ty: MonoType, sol: MutableMap<Int, MonoType> = solution): MonoType {
  return when (ty) {
    MonoType.BoolTy, MonoType.IntTy, MonoType.StringTy, is MonoType.Var -> ty
    is MonoType.FunType -> MonoType.FunType(applySolution(ty.arg, sol), applySolution(ty.returnTy, sol))
    is MonoType.Unknown -> sol[ty.u]?.let { applySolution(it, sol) } ?: ty
  }
}

fun applySolution(ty: PolyType): PolyType {
  return PolyType(ty.vars, applySolution(ty.mono))
}

fun testInfer(program: String) {
  unknownSupply = 0
  solution = mutableMapOf()
  val expr = Parser(Lexer(program)).parseExpression()
  val ctx: Context = initialContext
  val ty = applySolution(infer(ctx, expr))
  val poly = generalize(ctx, ty)
  println("$program : ${prettyPoly(poly)}")
//  println("Solution:")
//  solution.forEach { (t1, t2) -> println("$$t1 := ${prettyTy(t2)}")}
}

fun main() {
//  testInfer("10")
//  testInfer("true")
//  testInfer("if true then 10 else 20")
  testInfer(
    """
    let flip = \f => \x => \y => f y x in
    let f = \x => \y => if x == "Hello" then y else 10 in
    f
  """.trimIndent()
  )
//  testInfer("1 + 3")
//  testInfer("1 + 2 == 3 * 4")
//  testInfer(
//    """
//    let not = \x : Bool => if x then false else true in
//    let boolEq = \x : Bool => \y : Bool => if x then y else not y in
//    if boolEq (2 == 3) (3 == 4) then 10 else 20
//  """.trimIndent()
//  )
//  testInfer(
//    """
//    (\x => x) true
//  """.trimIndent()
//  )
}


