package example

object Tagless {
  object expressionProblem {
    // No access to this code => Bonus setup an SBT separate and do a compile version of the problem
    // how to add code without recompiling exiting code
    trait Expr
    case class Lit(b: Boolean) extends Expr
    case class Or(left: Expr, right: Expr) extends Expr
    case class And(left: Expr, right: Expr) extends Expr
    case class Not(expr: Expr) extends Expr
  }

  object Programs {
    import expressionProblem._

    val expression1 = And(
      Lit(true),
      Or(Lit(false), Lit(true))
    ) // true && (false || true) => true

  }

  object Interpreter {
    import expressionProblem._

    def eval(expr: Expr): Boolean = expr match {
      case And(left, right) => eval(left) && eval(right)
      case Lit(b)           => b
      case Not(expr)        => !eval(expr)
      case Or(left, right)  => eval(left) || eval(right)
    }
  }

  object Orchestrator {
    import expressionProblem._
    import Programs.expression1

    val value: Boolean = Interpreter.eval(expression1)
    // Intro to the problem => Extending to other Types the expression
    case class I(int: Int) extends Expr
    case class Sum(l: Expr, r: Expr) extends Expr
    // I am even able to create describe the computation
    val expression2: Sum = Sum(I(9), I(8))
    // Our clases will not be interpreted by library.
    // Library has to change the approach
    val value2: Boolean = Interpreter.eval(expression2)

  }

  def main(args: Array[String]) = println("Hello")
}
