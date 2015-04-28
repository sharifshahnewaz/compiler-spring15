import scala.util.matching.Regex

object Constants {
  val INTLIT: Regex = "[1-9][0-9]*|0".r
  var BOOLLIT: Regex = "true|false".r
  var IDENT: Regex = "[A-Z][A-Z0-9]*".r
  var KEYWORD: Regex = "[a-z]*".r
  var PROCEDURE: Regex = "readInt|writeInt".r
  val IntLitText: String = "IntLit"
  val BoolLitText: String = "BoolLit"
  val IdentText: String = "Ident"
  val KeywordText: String = "Keyword"
  val ProcedureText: String = "Procedure"
  val OperatorText: String = "Operator"
  val MultiplicativeText: String = "Multiplicative"
  val AdditiveText = "Additive"
  val CompareText = "Compare"

  var GRAPHVIZLINE: String = "%s [label=\"%s\",fillcolor=\"/x11/white\",shape=box]\n%s -> %s"

}