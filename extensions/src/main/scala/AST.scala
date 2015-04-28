import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.Queue

/**
 * File Name    :   AST.scala
 * Author       :   Joy Rahman, Sharif Shahnewaz, Tanvir Irfan, Imtiaz Muhammad
 * Purpose      :   Parse tokens and build AST
 */

class AST {

  var nodeListInFile = new ListBuffer[ASTNode]
  var symbolTable = HashMap.empty[String, String]
  var nodeNumber: Int = 0
  // this will be used to determine whether we are in declaration segment or not.
  // i am using this to differentiate between the ASTNode label. e.g. decl:'N' vs N 
  var isInDeclarationSegment: Boolean = true;
  var headOfAST: ASTNode = null;
  var travarsalQ: Queue[ASTNode] = new Queue[ASTNode]

  def getAbstractSyntextTree(tokenList: ListBuffer[Token]): ASTNode = {
    var remaining = tokenList //remaining list of the tokenlist
    nodeNumber += 1

    /**
     * Match a token type with first token of token list
     */
    def matchTokenType(tokenType: String): Boolean = {
      if (remaining.head.tokenType.equals(tokenType)) {
        return true;
      } else return false;
    }

    /**
     * Match a token value with first value token of token list
     */
    def matchTokenValue(tokenValue: String): Boolean = {
      // println("token to match " + tokenValue)
      if (remaining.head.value.equals(tokenValue)) {
        return true;
      } else return false;
    }

    /**
     * consume first token of the list and add token as a leaf of the tree with extra value string
     */
    def addChildrenAndConsumeTokenExtMessage(currentNode: ASTNode, extraValueString: String): ASTNode = {
      nodeNumber += 1
      var n: ASTNode = new ASTNode(currentNode, extraValueString + "'" + remaining.head.value + "'", remaining.head, nodeNumber, null, null)
      currentNode.childrens += (n)
      nodeListInFile += (n)
      remaining = remaining.tail
      return n
    }
    /**
     * consume first token of the list and add token as a leaf of the tree
     */
    def addChildrenAndConsumeToken(currentNode: ASTNode): ASTNode = {
      var extra = ""
      if (matchTokenType(Constants.IdentText)) {
        if (isInDeclarationSegment) extra = "decl: "
        addChildrenAndConsumeTokenExtMessage(currentNode, extra)
      } else if (matchTokenType(Constants.IntLitText)) {
        if (isInDeclarationSegment) extra = "num: "
        addChildrenAndConsumeTokenExtMessage(currentNode, extra)
      } else addChildrenAndConsumeTokenExtMessage(currentNode, "")

    }
    /**
     * crate a child ASTNode for the terminals
     */
    def getNodeAndConsumeToken(parent: ASTNode): ASTNode = {
      nodeNumber += 1
      var childNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
      nodeListInFile += (childNode)
      remaining = remaining.tail
      return childNode
    }
    /**
     * Match a token type and value with first value token of token list
     */
    def matchTokenTypeAndValue(tokenType: String, tokenValue: String): Boolean = {
      if (!matchTokenType(tokenType)) throw new ASTError("Expected " + tokenType + ", found " + remaining.head.tokenType + " at line " + remaining.head.lineNumber)
      if (!matchTokenValue(tokenValue)) throw new ASTError("Expected " + tokenType + ": " + tokenValue + ", found " + tokenType + ": " + remaining.head.value + " at line " + remaining.head.lineNumber)
      return true
    }

    /**
     * consume first token of the list
     */

    def consumeToken() {
      remaining = remaining.tail
    }

    def epsilonNode(parent: ASTNode): ASTNode = {
      nodeNumber += 1
      var n: ASTNode = new ASTNode(parent, "ε", null, nodeNumber, null, null)
      nodeListInFile += (n)
      return n
    }

    /**
     * Starting point of the program this creates and returns the dummy head ASTNode
     */

    def program(): ASTNode = {
      nodeNumber += 1
      var programNode = new ASTNode(null, "program", remaining.head, nodeNumber, null, null)
      headOfAST = programNode
      if (matchTokenTypeAndValue(Constants.KeywordText, "program")) {
        consumeToken()
        nodeListInFile += programNode
      }

      nodeNumber += 1
      var decListNode: ASTNode = new ASTNode(programNode, "decl list", null, nodeNumber, null, null)
      nodeListInFile += (decListNode)
      programNode.childrens += decListNode;

      declarations(decListNode)

      nodeNumber += 1
      var stmtListNode: ASTNode = new ASTNode(programNode, "stmt list", null, nodeNumber, null, null)
      nodeListInFile += (stmtListNode)
      programNode.childrens += stmtListNode;

      if (matchTokenTypeAndValue(Constants.KeywordText, "begin")) {
        consumeToken()
        isInDeclarationSegment = false
      }
      statementSequence(stmtListNode)

      return programNode
    }

    /**
     *      declarations()
     *      Recursively create subtree for declarations and return the head ASTNode
     *      <declarations> ::= VAR ident AS <type> SC <declarations> | ε
     */
    def declarations(parent: ASTNode): ASTNode = {

      var varName: String = null
      var varType: String = null

      while (!remaining.head.value.equals("begin")) {

        if (matchTokenTypeAndValue(Constants.KeywordText, "var")) {
          consumeToken()
        }
        var child: ASTNode = null;

        if (matchTokenType(Constants.IdentText)) {
          varName = remaining.head.value
          child = addChildrenAndConsumeToken(parent)
        }

        if (matchTokenTypeAndValue(Constants.KeywordText, "as")) {
          consumeToken()
        }

        if (matchTokenType(Constants.KeywordText)) {
          varType = remaining.head.value
          addChildrenAndConsumeToken(child)
        }

        if (matchTokenType(Constants.OperatorText) && matchTokenValue(";")) {
          consumeToken()
          if (!symbolTable.contains(varName)) {
            symbolTable += (varName -> varType)
          }
          else throw new ASTError("Multiple declaration of the varriable '"+varName+"' at line "+remaining.head.lineNumber)
        }
      }

      return parent
    }

    /**
     * Create subtree for statementSequence and return the head ASTNode
     */
    def statementSequence(parent: ASTNode): ASTNode = {
      if (!(remaining.head.value.equals("end") || remaining.head.value.equals("else"))) {
        statement(parent);
        if (matchTokenTypeAndValue(Constants.OperatorText, ";")) {
          consumeToken()
        }
        statementSequence(parent)
      }
      return parent
    }

    /**
     * Create subtree for statement and return the head ASTNode
     */
    def statement(parent: ASTNode): ASTNode = {
      if (matchTokenType(Constants.IdentText)) {
        assignment(parent)
      } else if (matchTokenValue("if")) {
        ifStatement(parent)
      } else if (matchTokenValue("while")) {
        whileStatement(parent)
      } else if (matchTokenValue("writeInt")) {
        writeInt(parent)
      }

      return parent
    }

    def ifStatement(parent: ASTNode): ASTNode = {

      nodeNumber += 1
      var ifNode: ASTNode = new ASTNode(parent, "if", remaining.head, nodeNumber, null, null)
      nodeListInFile += (ifNode)
      parent.childrens += ifNode
      if (matchTokenTypeAndValue(Constants.KeywordText, "if")) {
        consumeToken()
      }

      expression(ifNode);

      if (matchTokenTypeAndValue(Constants.KeywordText, "then")) {
        consumeToken()
      }
      nodeNumber += 1
      var stmtListNode: ASTNode = new ASTNode(ifNode, "stmt list", null, nodeNumber, null, null)
      nodeListInFile += (stmtListNode)
      ifNode.childrens += stmtListNode
      statementSequence(stmtListNode);

      elseClause(ifNode)

      if (matchTokenTypeAndValue(Constants.KeywordText, "end")) {
        consumeToken()
      }
      return ifNode;
    }

    def elseClause(parent: ASTNode): ASTNode = {
      if (matchTokenValue("else")) {
        nodeNumber += 1
        var stmtListNode: ASTNode = new ASTNode(parent, "stmt list", null, nodeNumber, null, null)
        nodeListInFile += (stmtListNode)
        parent.childrens += stmtListNode
        if (matchTokenTypeAndValue(Constants.KeywordText, "else")) {
          consumeToken()
        }
        statementSequence(stmtListNode)
      } else
        return null
    }

    def writeInt(parent: ASTNode): ASTNode = {
      nodeNumber += 1
      var writeNode: ASTNode = new ASTNode(parent, "writeInt", remaining.head, nodeNumber, null, null)
      nodeListInFile += (writeNode)
      parent.childrens += writeNode
      if (matchTokenTypeAndValue(Constants.ProcedureText, "writeInt")) {
        consumeToken()
      }
      expression(writeNode);
      return writeNode;
    }

    def whileStatement(parent: ASTNode): ASTNode = {
      nodeNumber += 1
      var whileNode: ASTNode = new ASTNode(parent, "while", remaining.head, nodeNumber, null, null)
      nodeListInFile += (whileNode)
      parent.childrens += whileNode
      if (matchTokenTypeAndValue(Constants.KeywordText, "while")) {
        consumeToken()
      }

      expression(whileNode);

      if (matchTokenTypeAndValue(Constants.KeywordText, "do")) {
        consumeToken()
      }
      nodeNumber += 1
      var stmtListNode: ASTNode = new ASTNode(whileNode, "stmt list", null, nodeNumber, null, null)
      nodeListInFile += (stmtListNode)
      whileNode.childrens += stmtListNode
      statementSequence(stmtListNode);

      if (matchTokenTypeAndValue(Constants.KeywordText, "end")) {
        consumeToken()
      }
      return whileNode;
    }

    def assignment(parent: ASTNode): ASTNode = {
      nodeNumber += 1
      var assignmentNode: ASTNode = new ASTNode(parent, ":=", remaining.head, nodeNumber, null, null)
      nodeListInFile += (assignmentNode)
      parent.childrens += assignmentNode
      if (matchTokenType(Constants.IdentText)) {
        addChildrenAndConsumeToken(assignmentNode)
      }
      if (matchTokenType(Constants.OperatorText) && matchTokenValue(":=")) {
        consumeToken()
      }
      assignmentPrime(assignmentNode)
      return assignmentNode
    }

    def assignmentPrime(parent: ASTNode): ASTNode = {
      if (matchTokenType(Constants.ProcedureText) && matchTokenValue("readInt")) {
        addChildrenAndConsumeToken(parent)
      } else
        expression(parent)
    }

    def expression(parent: ASTNode): ASTNode = {
      simpleExpression(parent)
      expressionPrime(parent)
      return parent
    }

    def simpleExpression(parent: ASTNode): ASTNode = {
      term(parent)
      simpleExpressionPrime(parent)
      return parent
    }

    def expressionPrime(parent: ASTNode): ASTNode = {
      if (matchTokenType(Constants.CompareText)) {
        nodeNumber += 1
        var compareNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
        nodeListInFile += (compareNode)
        //parent.childrens += compareNode

        var simpleExpNode: ASTNode = parent.childrens.remove(parent.childrens.length - 1)
        simpleExpNode.parent = compareNode
        parent.childrens += compareNode

        compareNode.childrens += simpleExpNode
        consumeToken()
        return expression(compareNode)
      } else return null

    }

    def simpleExpressionPrime(parent: ASTNode): ASTNode = {

      if (matchTokenType(Constants.AdditiveText)) {
        nodeNumber += 1
        var additiveNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
        nodeListInFile += (additiveNode)

        var lastNum: ASTNode = parent.childrens.remove(parent.childrens.length - 1)
        lastNum.parent = additiveNode
        parent.childrens += additiveNode
        additiveNode.childrens += lastNum
        consumeToken()
        //addChildrenAndConsumeToken(parent)
        return simpleExpression(additiveNode)
      } else return null
    }

    def term(parent: ASTNode): ASTNode = {
      factor(parent)
      termPrime(parent)
      return parent
    }

    /**
     * Create subtree for term prime and return the head ASTNode
     */
    def termPrime(parent: ASTNode): ASTNode = {

      if (matchTokenType(Constants.MultiplicativeText)) {
        nodeNumber += 1
        var termNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
        nodeListInFile += (termNode)

        var lastNum: ASTNode = parent.childrens.remove(parent.childrens.length - 1)
        lastNum.parent = termNode
        parent.childrens += termNode
        termNode.childrens += lastNum
        consumeToken()

        return term(termNode)
      } else return return null
    }

    /**
     * Create subtree for factor and return the head ASTNode
     */
    def factor(parent: ASTNode): ASTNode = {
      if (matchTokenType(Constants.IdentText)) {
        addChildrenAndConsumeToken(parent)
      } else if (matchTokenType(Constants.IntLitText)) {
        addChildrenAndConsumeToken(parent)
      } else if (matchTokenType(Constants.BoolLitText)) {
        addChildrenAndConsumeToken(parent)
      } else if (matchTokenTypeAndValue(Constants.OperatorText, "(")) {
        consumeToken()
        expression(parent)
        if (matchTokenTypeAndValue(Constants.OperatorText, ")"))
          consumeToken()
      }
      return parent;
    }

    def constractLineForGraphViz(parent: String, child: String, label: String): String = {
      var str: String = child + " [label=\"" + label + "\",fillcolor=\"/x11/white\",shape=box]\n" + parent + " -> " + child
      return str;
    }

    //calling the program function as a start point to create the AST
    program()
  }

  def buildGraphViz(astFileName: String) = {
    def constractLineForGraphViz(parent: String, child: String, label: String): String = {
      var str: String = child + " [label=\"" + label + "\",fillcolor=\"/x11/white\",shape=box]"
      if (parent.length() > 0) {
        str += "\n" + parent + " -> " + child
      }
      return str;
    }

    var resultStr: String = "digraph AST {\nordering=out;\nnode [shape = box, style = filled];\n"
    for (x <- nodeListInFile) {
      var s: String = ""
      if (x.parent != null) {
        s += constractLineForGraphViz(x.parent.nodeNumber + "", x.nodeNumber + "", x.nodeLabel + "[ " + x.nodeNumber + " ]")
      } else {
        s += constractLineForGraphViz("", x.nodeNumber + "", x.nodeLabel)
      }
      resultStr += s + "\n";
    }
    resultStr += "\n}"

    writeToFile(resultStr, astFileName)
    resultStr = ""
    nodeListInFile.clear()
  }

  def writeToFile(graphViz: String, fileName: String) {
    val pw = new PrintWriter(new File(fileName))
    pw.write(graphViz)
    pw.close
  }

  def generateGraphVizOuptutFile(astFileName: String) = {
    var resultStr: String = "digraph AST {\nordering=out;\nnode [shape = box, style = filled];\n"
    def constractLineForGraphViz(parent: String, child: String, label: String): String = {
      var str: String = child + " [label=\"" + label + "\",fillcolor=\"/x11/white\",shape=box]"
      if (parent.length() > 0) {
        str += "\n" + parent + " -> " + child
      }
      return str;
    }
    var count = 0
    travarsalQ.enqueue(headOfAST);
    while (!travarsalQ.isEmpty) {
      var n: ASTNode = travarsalQ.dequeue()

      var s: String = ""
      if (n.parent != null) {
        s += constractLineForGraphViz(n.parent.nodeNumber + "", n.nodeNumber + "", n.nodeLabel)
      } else {
        s += constractLineForGraphViz("", n.nodeNumber + "", n.nodeLabel)
      }
      resultStr += s + "\n";

      count += 1
      for (x <- n.childrens) {
        travarsalQ.enqueue(x)
      }
    }
    resultStr += "\n}"

    writeToFile(resultStr, astFileName)
    resultStr = ""
  }

}
