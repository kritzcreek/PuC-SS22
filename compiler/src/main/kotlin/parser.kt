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
  object LET : Token()
  object REC : Token()
  object IN : Token()

  object INT : Token()
  object BOOL : Token()
  object STRING : Token()

  // Symbols
  object LPAREN : Token()
  object RPAREN : Token()
  object ARROW : Token()
  object EQ_ARROW : Token()
  object BACKSLASH : Token()
  object EQUALS : Token()
  object COLON : Token()

  // Literal
  data class BOOL_LIT(val bool: Boolean) : Token()
  data class INT_LIT(val int: Int) : Token()
  data class STRING_LIT(val string: String): Token()

  data class IDENT(val ident: String) : Token()

  // Operator
  object PLUS : Token()
  object MINUS : Token()
  object MULTIPLY : Token()
  object DIVIDES : Token()
  object DOUBLE_EQUALS : Token()
  object HASH : Token()

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
      ':' -> Token.COLON
      '+' -> Token.PLUS
      '/' -> Token.DIVIDES
      '*' -> Token.MULTIPLY
      '#' -> Token.HASH
      '-' -> if (iter.peek() == '>') {
        iter.next()
        Token.ARROW
      } else {
        Token.MINUS
      }
      '=' -> if (iter.peek() == '>') {
        iter.next()
        Token.EQ_ARROW
      } else if (iter.peek() == '=') {
        iter.next()
        Token.DOUBLE_EQUALS
      } else {
        Token.EQUALS
      }
      '"' -> lexString()
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
      "let" -> Token.LET
      "rec" -> Token.REC
      "in" -> Token.IN
      "true" -> Token.BOOL_LIT(true)
      "false" -> Token.BOOL_LIT(false)
      "Int" -> Token.INT
      "Bool" -> Token.BOOL
      "String" -> Token.STRING
      else -> Token.IDENT(res)
    }
  }

  private fun lexString(): Token.STRING_LIT {
    var result = ""
    while (iter.peek() != '"') {
      val next = iter.next() ?: throw Error("Unterminated String Literal")
      result += next
    }
    iter.next()
    return Token.STRING_LIT(result)
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

  fun parseType(): MonoType {
    var ty = parseTypeAtom()
    while(lexer.lookahead() == Token.ARROW) {
      expect<Token.ARROW>("an arrow")
      ty = MonoType.FunType(ty, parseType())
    }
    return ty
  }

  fun parseTypeAtom(): MonoType {
    return when (val t = lexer.next()) {
      is Token.BOOL -> {
        MonoType.BoolTy
      }
      is Token.INT -> {
        MonoType.IntTy
      }
      is Token.STRING -> {
        MonoType.StringTy
      }
      is Token.LPAREN -> {
        val ty = parseType()
        expect<Token.RPAREN>("a closing paren")
        ty
      }
      else -> throw Error("Expected a type but got: $t")
    }
  }

  fun parseExpression(): Expr {
    return parseBinary(0)
  }

  fun parseBinary(minBindingPower: Int): Expr {
    var lhs = parseApplication()
    while (true) {
      val op = peekOperator() ?: break
      val (leftBp, rightBp) = bindingPowerForOp(op)
      if (minBindingPower > leftBp) break;
      lexer.next()
      val rhs = parseBinary(rightBp)
      lhs = Expr.Binary(lhs, op, rhs)
    }
    return lhs
  }

  private fun peekOperator(): Operator? {
    return when (lexer.lookahead()) {
      Token.DIVIDES -> Operator.Divide
      Token.DOUBLE_EQUALS -> Operator.Equality
      Token.MINUS -> Operator.Subtract
      Token.MULTIPLY -> Operator.Multiply
      Token.PLUS -> Operator.Add
      Token.HASH -> Operator.Concat
      else -> null
    }
  }

  private fun bindingPowerForOp(op: Operator): Pair<Int, Int> {
    return when (op) {
      Operator.Equality -> 2 to 1
      Operator.Add, Operator.Subtract, Operator.Concat -> 3 to 4
      Operator.Multiply, Operator.Divide -> 5 to 6
    }
  }

  fun parseApplication(): Expr {
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
      is Token.STRING_LIT -> parseString()
      is Token.BACKSLASH -> parseLambda()
      is Token.LET -> parseLet()
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

  private fun parseString(): Expr {
    val t = expect<Token.STRING_LIT>("string")
    return Expr.StringLiteral(t.string)
  }

  private fun parseLet(): Expr {
    expect<Token.LET>("let")
    val recursive = lexer.lookahead() == Token.REC
    if (recursive) {
      expect<Token.REC>("rec")
    }
    val binder = expect<Token.IDENT>("binder").ident
    expect<Token.EQUALS>("equals")
    val expr = parseExpression()
    expect<Token.IN>("in")
    val body = parseExpression()
    return Expr.Let(recursive, binder, expr, body)
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
    var tyBinder: MonoType? = null
    if (lexer.lookahead() == Token.COLON) {
      expect<Token.COLON>("colon")
      tyBinder = parseType()
    }
    expect<Token.EQ_ARROW>("arrow")
    val body = parseExpression()
    return Expr.Lambda(binder.ident, tyBinder, body)
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

fun monoTy(input: String): MonoType {
  return Parser(Lexer(input)).parseType()
}

fun testLex(input: String) {
  val lexer = Lexer(input)
  do {
    println(lexer.next())
  } while (lexer.lookahead() != Token.EOF)
}

fun testParse(input: String) {
  val parser = Parser(Lexer(input))
  val expr = parser.parseExpression()
  print(expr)
}

fun test(input: String) {
  val parser = Parser(Lexer(input))
  val expr = parser.parseExpression()
  print(
    eval(
      persistentHashMapOf(), expr
    )
  )
}

// Hausaufgabe: Definiere Binaere Operatoren fuer
// - && Boolsches Und  x && y || z == (x && y) || z
// - || Boolsches Oder
// - ^ Exponent

fun main() {
//  testLex("""-> => == / * + -""")
  testLex(
    """
      "Hello"
   """.trimMargin()
  )
}