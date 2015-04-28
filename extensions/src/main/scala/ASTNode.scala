import scala.collection.mutable.ListBuffer
/**
 * A node in the parse tree
 */
class ASTNode(var parent: ASTNode, var nodeLabel: String, var token: Token, var nodeNumber: Int,var st: ASTNode, var ptr: ASTNode){
  var childrens: ListBuffer[ASTNode] = new ListBuffer[ASTNode] //list of child nodes
  override def toString :String = {
    var res : String = this.nodeLabel +"[ "
    for(x <- this.childrens) {
      res += x.nodeLabel + ", "
    }
    return res + " ]"
  }
}