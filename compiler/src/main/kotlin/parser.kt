import kotlinx.collections.immutable.persistentHashMapOf

sealed class Token {

  @Override
  override fun toString(): String {
    return this.javaClass.simpleName
  }

  // Keyword
  object IF : Token()
  object THEN : Token()
  object ELSE : Token()

  // Symbols
  object LPAREN : Token()
  object RPAREN : Token()
  object ARROW : Token()
  object BACKSLASH : Token()

  // Literal
  data class BOOL_LIT(val bool: Boolean) : Token()
  data class INT_LIT(val int: Int) : Token()

  data class IDENT(val ident: String) : Token()

  // Operator
  object PLUS : Token()

  // Control
  object EOF : Token()
}

class PeekableIterator<T>(val iter: Iterator<T>) {
  var lh: T? = null
  fun peek(): T? {
    lh = next()
    return lh
  }

  fun next(): T? {
    lh?.let { lh = null; return it }
    return if (iter.hasNext()) {
      iter.next()
    } else {
      null
    }
  }
}

class Lexer(input: String) {

  private val iter = PeekableIterator(input.iterator())
  var lh: Token? = null

  fun next(): Token {
    chompWhitespace()
    lh?.let { it -> lh = null; return it }
    return when (val c = iter.next()) {
      null -> Token.EOF
      '(' -> Token.LPAREN
      ')' -> Token.RPAREN
      '\\' -> Token.BACKSLASH
      '+' -> Token.PLUS
      '-', '=' -> if (iter.next() == '>') {
        Token.ARROW
      } else {
        throw Exception("Tried to parse an Arrow but failed")
      }
      else -> when {
        c.isJavaIdentifierStart() -> lexIdentifier(c)
        c.isDigit() -> lexInt(c)
        else -> throw Exception("Unexpected $c")
      }
    }
  }

  private fun lexInt(first: Char): Token {
    var res = first.toString()
    while (iter.peek()?.isDigit() == true) {
      res += iter.next()
    }
    return Token.INT_LIT(res.toInt())
  }

  private fun lexIdentifier(first: Char): Token {
    var res = first.toString()
    while (iter.peek()?.isJavaIdentifierPart() == true) {
      res += iter.next()
    }
    return when (res) {
      "if" -> Token.IF
      "then" -> Token.THEN
      "else" -> Token.ELSE
      "true" -> Token.BOOL_LIT(true)
      "false" -> Token.BOOL_LIT(false)
      else -> Token.IDENT(res)
    }
  }

  private fun chompWhitespace() {
    while (iter.peek()?.isWhitespace() == true) {
      iter.next()
    }
  }

  public fun lookahead(): Token {
    lh = next()
    return lh ?: Token.EOF
  }
}

class Parser(val lexer: Lexer) {
  fun parseExpression(): Expr {
    var expr = parseAtom() ?: throw Exception("Expected an expression")
    while (true) {
      val arg = parseAtom() ?: break
      expr = Expr.App(expr, arg)
    }
    return expr
  }

  fun parseAtom(): Expr? {
    return when (lexer.lookahead()) {
      is Token.INT_LIT -> parseInt()
      is Token.BOOL_LIT -> parseBool()
      is Token.BACKSLASH -> parseLambda()
      is Token.IF -> parseIf()
      is Token.IDENT -> parseVar()
      is Token.LPAREN -> {
        expect<Token.LPAREN>("opening paren")
        val inner = parseExpression()
        expect<Token.RPAREN>("closing paren")
        inner
      }
      else -> null
    }
  }

  private fun parseVar(): Expr.Var {
    val ident = expect<Token.IDENT>("identifier")
    return Expr.Var(ident.ident)
  }

  private fun parseIf(): Expr.If {
    expect<Token.IF>("if")
    val condition = parseExpression()
    expect<Token.THEN>("then")
    val thenBranch = parseExpression()
    expect<Token.ELSE>("else")
    val elseBranch = parseExpression()
    return Expr.If(condition, thenBranch, elseBranch)
  }

  private fun parseLambda(): Expr.Lambda {
    expect<Token.BACKSLASH>("lambda")
    val binder = expect<Token.IDENT>("binder")
    expect<Token.ARROW>("arrow")
    val body = parseExpression()
    return Expr.Lambda(binder.ident, body)
  }

  private fun parseInt(): Expr.IntLiteral {
    val t = expect<Token.INT_LIT>("integer")
    return Expr.IntLiteral(t.int)
  }

  private fun parseBool(): Expr.BoolLiteral {
    val t = expect<Token.BOOL_LIT>("boolean")
    return Expr.BoolLiteral(t.bool)
  }

  private inline fun <reified T> expect(msg: String): T {
    val tkn = lexer.next()
    return tkn as? T ?: throw Exception("Expected $msg but saw $tkn")
  }
}

fun testLex(input: String) {
  val lexer = Lexer(input)
  do {
    println(lexer.next())
  } while (lexer.lookahead() != Token.EOF)
}

val add = Value.Closure(persistentHashMapOf(), "x", Expr.Lambda("y", Expr.Addition(x, y)))
val minusOne = Value.Int(-1)
val minusTwo = Value.Int(-2)
val equals = Value.Closure(persistentHashMapOf(), "x", Expr.Lambda("y", Expr.Equality(x, y)))

fun test(input: String) {
  val parser = Parser(Lexer(input))
  val expr = parser.parseExpression()
  print(
    eval(
      persistentHashMapOf(
        "add" to add,
        "equals" to equals,
        "z" to eval(emptyEnv, z),
        "minusOne" to minusOne,
        "minusTwo" to minusTwo,
      ), expr
    )
  )
}

// Hausaufgabe (normal): Zeilenkommentare // asd
// Hausaufgabe (hard): Blockkommentare /* asd */
// Hausaufgabe (hard++): Nested Blockkommentare /* asd /* ok */ */

fun main() {
  // testLex("""if 3 then 4 else 5""")
  test(
    """
    z (\self => \x ->
    if equals x 0 then
      0
    else if equals x 1 then
      1
    else
      add (self (add x minusOne)) (self (add x minusTwo))
    ) 10
   """.trimMargin()
  )
}