package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  //private var stackSet = Set[String]()

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.transform((name, signalExpr) => Signal {
      evalInner(signalExpr(), namedExpressions, Set.empty[String] + name)
    })
  }

  def evalInner(expr: Expr, references: Map[String, Signal[Expr]], stackSet: Set[String]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => {
      if (stackSet.contains(name)) Double.NaN
      else {
        evalInner(getReferenceExpr(name, references), references, stackSet + name)
      }
    }
    case Plus(a: Expr, b: Expr) => evalInner(a, references, stackSet) + evalInner(b, references, stackSet)
    case Minus(a: Expr, b: Expr) => evalInner(a, references, stackSet) - evalInner(b, references, stackSet)
    case Times(a: Expr, b: Expr) => evalInner(a, references, stackSet) * evalInner(b, references, stackSet)
    case Divide(a: Expr, b: Expr) => evalInner(a, references, stackSet) / evalInner(b, references, stackSet)
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
