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

  object libraryExtensionTag {

    sealed abstract class Expr(val tag: String)
    case class B(boolean: Boolean) extends Expr("bool")
    case class Or(left: Expr, right: Expr) extends Expr("bool")
    case class And(left: Expr, right: Expr) extends Expr("bool")
    case class Not(expr: Expr) extends Expr("bool")
    case class I(int: Int) extends Expr("int")
    case class Sum(left: Expr, right: Expr) extends Expr("int")

    def eval_2(newTrait: Expr): Any = newTrait match {
      case B(boolean) => boolean
      case Or(left, right) =>
        if (left.tag == "bool" && right.tag == "bool")
          eval_2(left).asInstanceOf[Boolean] || eval_2(right)
            .asInstanceOf[Boolean]
      case And(left, right) =>
        if (left.tag == "bool" && right.tag == "bool")
          eval_2(left).asInstanceOf[Boolean] && eval_2(right)
            .asInstanceOf[Boolean]
      case Not(expr) =>
        if (expr.tag == "bool") !eval_2(expr).asInstanceOf[Boolean]
      case I(int) => int
      case Sum(left, right) =>
        eval_2(left).asInstanceOf[Int] + eval_2(right).asInstanceOf[Int]
    }
    // summary solves de problem but we loose compile type safety
  }

  object TaglessInitial {
    trait Expr[A]
    case class B(boolean: Boolean) extends Expr[Boolean]
    case class Or(left: Expr[Boolean], right: Expr[Boolean])
        extends Expr[Boolean]
    case class And(left: Expr[Boolean], right: Expr[Boolean])
        extends Expr[Boolean]
    case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
    case class I(int: Int) extends Expr[Int]
    case class Sum(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

    def eval[A](expr: Expr[A]): A = expr match {
      case And(left, right) => eval(left) && eval(right)
      case B(boolean)       => boolean
      case I(int)           => int
      case Not(expr)        => !eval(expr)
      case Or(left, right)  => eval(left) || eval(right)
      case Sum(left, right) => eval(left) + eval(right)
    }

    val test = eval(Or(B(true), And(B(true), B(false))))
    val test2 = eval(Sum(I(24), I(-3)))
  }

  def main(args: Array[String]) = println("Hello")
}
