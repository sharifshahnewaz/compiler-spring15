import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File

/**
 * @author Sharif-MSIf
 */
class ILOC {
  var regCounter: Int = -1
  var blockCounter: Int = 0

  var blocksInFile = new ListBuffer[BasicBlock]
  var cfgEdges = new ListBuffer[String]
  var environment = HashMap.empty[String, EnvVar]
  var operatorToILOC = HashMap.empty[String, String]
  var exitBlock: BasicBlock = null
  var fullILOCCOdes = ""

  def generateILOC(root: ASTNode, symbolTable: HashMap[String, String]) = {

    def getFreshReg(): String = {
      regCounter += 1
      return "r" + regCounter
    }

    def getNewBlockName(): String = {
      blockCounter += 1
      return "B" + blockCounter
    }

    symbolTable.keys.foreach { varriable =>
      environment += (varriable -> new EnvVar(varriable, symbolTable(varriable), "r_" + varriable))
    }

    operatorToILOC += ("*" -> "mult")
    operatorToILOC += ("div" -> "div")
    operatorToILOC += ("+" -> "add")
    operatorToILOC += ("-" -> "sub")
    operatorToILOC += ("mod" -> "mod")
    operatorToILOC += ("=" -> "cmp_EQ")
    operatorToILOC += ("!=" -> "cmp_NE")
    operatorToILOC += ("<" -> "cmp_LT")
    operatorToILOC += (">" -> "cmp_GT")
    operatorToILOC += ("<=" -> "cmp_LE")
    operatorToILOC += (">=" -> "cmp_GE")

    def program() {
      declarations(root.childrens.toList.head)
    }

    def declarations(node: ASTNode) {

      var programEntryBlock = new BasicBlock(getNewBlockName(), "")
      exitBlock = programEntryBlock;

      blocksInFile += programEntryBlock

      cfgEdges += "entry -> " + programEntryBlock.blockName

      environment.keys.foreach { envVar =>
        if (environment(envVar).varType.equals("int")) {
          programEntryBlock.ILOCCodeSeq += "loadI 0 => " + environment(envVar).assignedReg + "\n"
        } else {
          programEntryBlock.ILOCCodeSeq += "loadI false => " + environment(envVar).assignedReg + "\n"
        }
      }

      //currentBlock = programEntryBlock;
      stmtList(root.childrens.toList.tail.head, programEntryBlock)
      exitBlock.ILOCCodeSeq += "exit\n"
      cfgEdges += exitBlock.blockName + "->exit"
    }

    def stmtList(stmtListNode: ASTNode, entryBlock: BasicBlock): BasicBlock = {
      exitBlock = entryBlock
      for (node <- stmtListNode.childrens) {
        exitBlock = statement(node, exitBlock)
      }
      return exitBlock
    }

    def statement(stmtNode: ASTNode, entryBlock: BasicBlock): BasicBlock = {
      if (stmtNode.nodeLabel.equals(":=")) {
        return assignment(stmtNode, entryBlock)
      } else if (stmtNode.nodeLabel.equals("if")) {
        return ifStatement(stmtNode, entryBlock)
      } else if (stmtNode.nodeLabel.equals("while")) {
        return whileStatement(stmtNode, entryBlock)
      } else if (stmtNode.nodeLabel.equals("writeInt")) {
        return writeInt(stmtNode, entryBlock)
      }
      return null
    }

    def assignment(node: ASTNode, entryBlock: BasicBlock): BasicBlock = {
      if (node.childrens.toList.tail.head.token.value.equals("readInt")) {
        entryBlock.ILOCCodeSeq += "readInt => " + environment(node.childrens.toList.head.token.value).assignedReg + "\n"
      } else {
        var x = expression(node.childrens.toList.tail.head, entryBlock)
        entryBlock.ILOCCodeSeq += "i2i " + x +
          " => " + environment(node.childrens.toList.head.token.value).assignedReg + "\n"
      }
      return entryBlock

    }

    def ifStatement(node: ASTNode, entryBlock: BasicBlock): BasicBlock = {
      var ifBodyBlock = new BasicBlock(getNewBlockName(), "")
      var elseBlock: BasicBlock = null
      var nextBlock: BasicBlock = null

      blocksInFile += ifBodyBlock
      if (node.childrens.length > 2) {
        elseBlock = new BasicBlock(getNewBlockName(), "")
        blocksInFile += elseBlock
      }
      var ifExitBlock = new BasicBlock(getNewBlockName(), "")
      blocksInFile += ifExitBlock
      var x = expression(node.childrens.toList.head, entryBlock)
      if (node.childrens.length > 2) {
        nextBlock = elseBlock

      } else { nextBlock = ifExitBlock }

      entryBlock.ILOCCodeSeq += "cbr " + x + " -> " + ifBodyBlock.blockName + ", " + nextBlock.blockName + "\n"
      cfgEdges += entryBlock.blockName + " -> " + ifBodyBlock.blockName
      cfgEdges += entryBlock.blockName + " -> " + nextBlock.blockName

      //currentBlock = ifBlock
      var ifBodyExit = stmtList(node.childrens.toList.tail.head, ifBodyBlock)
      ifBodyExit.ILOCCodeSeq += "jumpI -> " + ifExitBlock.blockName + "\n"
      cfgEdges += ifBodyExit.blockName + " -> " + ifExitBlock.blockName
      if (node.childrens.length > 2) {
        //currentBlock = elseBlock
        var elseExit = stmtList(node.childrens.toList.tail.tail.head, elseBlock)
        elseExit.ILOCCodeSeq += "jumpI -> " + ifExitBlock.blockName + "\n"
        cfgEdges += elseExit.blockName + " -> " + ifExitBlock.blockName
      }
      return ifExitBlock
    }
    def whileStatement(node: ASTNode, entryBlock: BasicBlock): BasicBlock = {
      var conditionBlock: BasicBlock = null
      if (entryBlock.ILOCCodeSeq.length() > 0) {
        conditionBlock = new BasicBlock(getNewBlockName(), "")
        blocksInFile += conditionBlock
        entryBlock.ILOCCodeSeq += "jumpI -> " + conditionBlock.blockName + "\n"
        cfgEdges += entryBlock.blockName + " -> " + conditionBlock.blockName
      } else { conditionBlock = entryBlock }

      var bodyBlock = new BasicBlock(getNewBlockName(), "")
      blocksInFile += bodyBlock

      var x = expression(node.childrens.toList.head, conditionBlock)
      var whileExitBlock = new BasicBlock(getNewBlockName(), "")
      blocksInFile += whileExitBlock
      conditionBlock.ILOCCodeSeq += "cbr " + x + " -> " + bodyBlock.blockName + ", " + whileExitBlock.blockName + "\n"
      cfgEdges += conditionBlock.blockName + " -> " + bodyBlock.blockName
      cfgEdges += conditionBlock.blockName + " -> " + whileExitBlock.blockName

      //currentBlock = bodyBlock
      var bodyExitBlock = stmtList(node.childrens.toList.tail.head, bodyBlock)
      bodyExitBlock.ILOCCodeSeq += "jumpI -> " + conditionBlock.blockName + "\n"
      cfgEdges += bodyExitBlock.blockName + " -> " + conditionBlock.blockName
      //currentBlock = exitBlock

      return whileExitBlock
    }
    def writeInt(node: ASTNode, entryBlock: BasicBlock): BasicBlock = {
      var x = expression(node.childrens.toList.head, entryBlock) + "\n"
      entryBlock.ILOCCodeSeq += "writeInt " + x

      return entryBlock;
    }

    def expression(node: ASTNode, block: BasicBlock): String = {
      if (node.token.tokenType == Constants.CompareText) {
        var resultReg = getFreshReg()
        var se = simpleExpression(node.childrens.toList.head, block)
        var exp = expression(node.childrens.toList.tail.head, block)
        block.ILOCCodeSeq += operatorToILOC(node.token.value) + " " + se + ", " + exp + " => " + resultReg + "\n"
        return resultReg
      } else return simpleExpression(node, block)
    }

    def simpleExpression(node: ASTNode, block: BasicBlock): String = {
      if (node.token.tokenType == Constants.AdditiveText) {
        var resultReg = getFreshReg()
        //term and simple expression is switched
        var se = simpleExpression(node.childrens.toList.head, block)
        var ter = term(node.childrens.toList.tail.head, block)

        block.ILOCCodeSeq += operatorToILOC(node.token.value) + " " + se + ", " + ter + " => " + resultReg + "\n"

        return resultReg
      } else
        return term(node, block)
    }

    def term(node: ASTNode, block: BasicBlock): String = {

      if (node.token.tokenType.equals(Constants.MultiplicativeText)) {

        var resultReg = getFreshReg()
        //term and fac are switched
        var ter = term(node.childrens.toList.head, block)
        var fac = factor(node.childrens.toList.tail.head, block)

        block.ILOCCodeSeq += operatorToILOC(node.token.value) + " " + ter + ", " + fac + " => " + resultReg + "\n"

        return resultReg
      } else return factor(node, block)
    }

    def factor(node: ASTNode, block: BasicBlock): String = {
      if (node.token.tokenType == Constants.IdentText) {
        return environment(node.token.value).assignedReg
      } else {
        var resultReg = getFreshReg()
        block.ILOCCodeSeq += "loadI " + node.token.value + " => " + resultReg + "\n"
        return resultReg
      }
      return null

    }

    program()
    //printCode()

  }

  def constractLineForGraphViz(parent: String, child: String, label: String): String = {
    var str: String = child + " [label=\"" + label + "\",fillcolor=\"/x11/white\",shape=box]\n" + parent + " -> " + child
    return str;
  }
  def buildGraphViz(ilocFileName: String) = {
    def constractLineForGraphViz(block: BasicBlock): String = {

      var str: String = block.blockName + " [label=\"" + block.blockName + "\\n\\n" + block.ILOCCodeSeq.replaceAllLiterally("\n", "\\n") + "\",fillcolor=\"/x11/white\",shape=box]"

      return str;
    }

    var resultStr: String = "digraph ILOC {ordering=out;\nnode [shape = none];\nedge [tailport = s];\n"
    for (x <- blocksInFile) {
      var s: String = constractLineForGraphViz(x)
      fullILOCCOdes += x.blockName + ":" + "\n" + x.ILOCCodeSeq
      resultStr += s + "\n";
    }

    for (x <- cfgEdges) {
      resultStr += x + "\n"
    }
    resultStr += "\n}"
    writeToFile(resultStr, ilocFileName)
    resultStr = ""
    blocksInFile.clear()
  }
  def writeToFile(graphViz: String, fileName: String) {
    val pw = new PrintWriter(new File(fileName))
    pw.write(graphViz)
    pw.close
  }

  def printCode() {
    for (block <- blocksInFile) {
      if (block.ILOCCodeSeq.length() > -1) {
        println(block.blockName + ":")
        print(block.ILOCCodeSeq)
      }
    }
  }

}