import scala.collection.mutable.HashMap

/**
 * File Name    :   TypeChecker.scala
 * Author       :   Joy Rahman, Sharif Shahnewaz, Tanvir Irfan, Imtiaz Muhammad
 * Purpose      :   Check type of the AST statements
 */

class TypeChecker {

  var symbolTable = HashMap.empty[String, String]

  def verifyChildrenCount(node: ASTNode, childrenLength: Int) {
    if (node.childrens.length != childrenLength) {
      throw new TypeError("AST format exception")
    }
  }

  def checkProgram(rootNode: ASTNode, symbolTables: HashMap[String, HashMap[String, String]]): Boolean = {

    var procedureList = rootNode.childrens.toList.tail.head.childrens

    for (procedure <- procedureList) {
      var procedureName = procedure.nodeLabel.split(" ")(1)
      symbolTable = symbolTables(procedureName)
      if (!statementList(procedure.childrens.tail.tail.head)) {
        throw new TypeError("Program has a type error")
      }

    }
    symbolTable = symbolTables("program")
    if (statementList(rootNode.childrens.toList.tail.tail.head))
      return true;
    else
      throw new TypeError("Program has a type error")

  }

  def statementList(stmtListNode: ASTNode): Boolean = {
    var retVal: Boolean = true

    if (stmtListNode.childrens.length > 0) {
      var remaining = stmtListNode.childrens.toList
      while (remaining.length > 0) {
        retVal = retVal && statement(remaining.head)
        remaining = remaining.tail
      }
    }
    return retVal
  }

  def statement(stmtNode: ASTNode): Boolean = {

    //check while if etc condition
    if (stmtNode.nodeLabel.equals(":=")) {
      return assignment(stmtNode)
    } else if (stmtNode.nodeLabel.equals("if")) {
      return ifStatement(stmtNode)
    } else if (stmtNode.nodeLabel.equals("while")) {
      return whileStatement(stmtNode)
    } else if (stmtNode.nodeLabel.equals("writeInt")) {
      return writeInt(stmtNode)
    } else if (stmtNode.nodeLabel.startsWith("procCall")) {
      return procCall(stmtNode)
    } else throw new TypeError("Statement has a type mismatch")
  }
  def procCall(node: ASTNode): Boolean = {
    return true
  }
  def assignment(node: ASTNode): Boolean = {

    var leftChild = node.childrens.toList.head;

    var rightChild = node.childrens.toList.tail.head;

    if (getExpressionType(leftChild) == getExpressionType(rightChild))
      return true
    else
      throw new TypeError("Can not use operator '" + node.nodeLabel + "' from '" + getExpressionType(rightChild) + "' to '" + getExpressionType(leftChild) + "' at line " + node.token.lineNumber)
  }

  def ifStatement(node: ASTNode): Boolean = {
    var retVal: Boolean = true
    if (node.childrens.length > 0) {
      if (getExpressionType(node.childrens.toList.head).equals("bool")) {
        retVal = retVal && true
      } else
        throw new TypeError("Expression does not return 'bool' for 'if' statement at line: " + node.childrens.toList.head.token.lineNumber)
    }
    if (node.childrens.length > 1) {
      retVal = retVal && statementList(node.childrens.toList.tail.head)
    }
    if (node.childrens.length > 2) {
      retVal = retVal && statementList(node.childrens.toList.tail.tail.head)
    }

    return retVal;
  }

  def whileStatement(node: ASTNode): Boolean = {
    var retVal: Boolean = true
    if (node.childrens.length > 0) {
      if (getExpressionType(node.childrens.toList.head).equals("bool")) {
        retVal = retVal && true
      } else
        throw new TypeError("Expression does not return 'bool' for 'while' statement at line: " + node.childrens.toList.head.token.lineNumber)
    }
    if (node.childrens.length > 1) {
      retVal = retVal && statementList(node.childrens.toList.tail.head)
    }
    return retVal;
  }

  def writeInt(node: ASTNode): Boolean = {
    if (getExpressionType(node.childrens.toList.head).equals("int"))
      return true
    else
      throw new TypeError("writeInt is not applicable for 'bool' at line " + node.token.lineNumber)
  }

  def getExpressionType(node: ASTNode): String = {

    //checking for leaf node
    if (node.childrens.length == 0) {

      if (node.token.tokenType.equals(Constants.IntLitText) || node.token.value.equals("readInt")) {
        return "int";
      } else if (node.token.tokenType.equals(Constants.IdentText)) {
        if (symbolTable.contains(node.token.value))
          return symbolTable(node.token.value)
        else
          throw new TypeError("Varriable '" + node.token.value + "' is not declared")
      } else if (node.token.tokenType.equals(Constants.BoolLitText)) {
        return "bool";
      } else throw new TypeError("Unexpected value"); ;

    } //checking for internal node
    else {

      var leftChild = node.childrens.toList.head
      var rightChild = node.childrens.toList.tail.head

      var tokenType = node.token.tokenType
      var tokenValue = node.token.value
      if (tokenType.equals(Constants.CompareText) || tokenType.equals(Constants.AdditiveText) ||
        tokenType.equals(Constants.MultiplicativeText)) {
        if (!getExpressionType(leftChild).equals("int")) {
          throw new TypeError(getExpressionType(leftChild) + " is not a valid left operand type of " + tokenValue + " at line: " + node.token.lineNumber)
        }
        if (!getExpressionType(rightChild).equals("int")) {
          throw new TypeError(getExpressionType(rightChild) + " is not a valid right operand type of " + tokenValue + " at line: " + node.token.lineNumber)
        }
        if (tokenType.equals(Constants.CompareText)) {
          return "bool"
        } else
          return "int"
      }
    }
    return null;
  }
}