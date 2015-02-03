
// Modifying only the dup methods, 
// change this code so that it will replace any expression that is either
// two times a subexpression, or a subexpression times two, with
// the sum of that subexpression with itself.

object warmup {
  sealed abstract trait Expr
  case class Plus(e1: Expr, e2:Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr
  case class Num(value: Int) extends Expr
  
  def eval(e: Expr):Int = e match {
    case Plus(e1,e2)  => eval(e1) + eval(e2)
    case Times(e1,e2) => eval(e1) * eval(e2)
    case Num(value)   => value
  }
  
  def str(e: Expr):String = e match {
    case Plus(e1,e2)   => s"(+ ${str(e1)} ${str(e2)})"
    case Times(e1, e2) => s"(* ${str(e1)} ${str(e2)})"
    case Num(value)    => value.toString
  }
  
  def copy(e: Expr):Expr = e match {
    case Plus(e1,e2)   => Plus(copy(e1), copy(e2)) 
    case Times(e1, e2) => Times(copy(e1), copy(e2))
    case Num(value)    => Num(value)
  }
  
  def dup(e: Expr):Expr = e match {
    case Plus(e1,e2)   => Plus(dup(e1), dup(e2)) 
    case Times(e1, e2) => Times(dup(e1), dup(e2))
    case Num(value)    => Num(value)
  }
  
  // One simple test case:
  // (* 2 (+ 1 (* 3 2))) should duplicate to (+ (+ 1 (+ 3 3)) (+ 1 (+ 3 3)))
  def main(argv: Array[String]) {
    val e : Expr = Times(Num(2), Plus(Num(1),Times(Num(3),Num(2))))
    println(s"dup(${str(e)}) -> ${str(dup(e))} => ${eval(copy(e))}")
  }
}