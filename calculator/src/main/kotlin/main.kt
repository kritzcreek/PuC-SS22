sealed class Expression {
    data class Addition(val summand1: Expression, val summand2: Expression) : Expression()
    data class Subtraction(val x: Expression, val y: Expression) : Expression()
    data class Multiplication(val factor1: Expression, val factor2: Expression): Expression()
    data class Number(val number: Int) : Expression()
    data class Negation(val expr: Expression): Expression()
}

sealed class SimplerExpression {
    data class Addition(val summand1: SimplerExpression, val summand2: SimplerExpression) : SimplerExpression()
    data class Subtraction(val x: SimplerExpression, val y: SimplerExpression) : SimplerExpression()
    data class Multiplication(val factor1: SimplerExpression, val factor2: SimplerExpression): SimplerExpression()
    data class Number(val number: Int) : SimplerExpression()
}

fun simplify(expression: Expression): SimplerExpression {
    return when(expression) {
        is Expression.Addition -> SimplerExpression.Addition(
            simplify(expression.summand1),
            simplify(expression.summand2)
        )
        is Expression.Multiplication -> SimplerExpression.Multiplication(
            simplify(expression.factor1),
            simplify(expression.factor2)
        )
        is Expression.Negation -> SimplerExpression.Subtraction(
            SimplerExpression.Number(0),
            simplify(expression.expr)
        )
        is Expression.Number -> SimplerExpression.Number(expression.number)
        is Expression.Subtraction -> SimplerExpression.Subtraction(
            simplify(expression.x),
            simplify(expression.y)
        )
    }
}

fun interpSimple(expression: SimplerExpression): Int {
    return when (expression) {
        is SimplerExpression.Addition ->
            interpSimple(expression.summand1) + interpSimple(expression.summand2)
        is SimplerExpression.Number ->
            expression.number
        is SimplerExpression.Multiplication ->
            interpSimple(expression.factor1) * interpSimple(expression.factor2)
        is SimplerExpression.Subtraction ->
            interpSimple(expression.x) - interpSimple(expression.y)
    }
}

fun interp(expr: Expression): Int {
    return interpSimple(simplify(expr))
}

fun main(args: Array<String>) {
    val addition = Expression.Subtraction(
        Expression.Number(1),
        Expression.Multiplication(
            Expression.Number(3),
            Expression.Negation(Expression.Number(4))
        )
    )
    println("${addition} = ${interp(addition)}")
}