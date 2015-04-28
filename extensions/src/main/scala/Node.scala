import scala.collection.mutable.ListBuffer
/**
 * A node in the parse tree
 */
class Node(var parent: Node, var nodeLabel: String, var token: Token, var nodeNumber: Int) {
  var childrens: ListBuffer[Node] = new ListBuffer[Node] //list of child nodes
  override def toString :String = {
    var res : String = this.nodeLabel +"[ "
    for(x <- this.childrens) {
      res += x.nodeLabel + ", "
    }
    return res + " ]"
  }
}