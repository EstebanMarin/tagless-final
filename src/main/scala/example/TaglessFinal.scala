package example

object Tagless {
  object expressionProblem {
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



  def main(args: Array[String]) = println("Hello")
}
