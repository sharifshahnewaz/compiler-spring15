import scala.util.matching.Regex
/*
 * 
 * import scala.util.matching.Regex
val pattern = "Scala".r // <=> val pattern = new Regex("Scala")
val str = "Scala is very cool"
val result = pattern findFirstIn str
result match {
  case Some(v) => println(v)
  case _ =>
} // output: Scala
 * 
 * */

/**
 * @author joy
 */
object regex_test {

  def main(args: Array[String]) {
    val pattern = "Scala".r
    val str = "we love Scala"
    val result = pattern findFirstIn (str)
    println(result)

  }
}