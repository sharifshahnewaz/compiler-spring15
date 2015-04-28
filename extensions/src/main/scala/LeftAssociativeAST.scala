import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.Queue

/**
 * File Name    :   AST.scala
 * Author       :   Joy Rahman, Sharif Shahnewaz, Tanvir Irfan Chowdhury, Imtiaz Muhammad
 * Purpose      :   Parse tokens and build AST
 */

class LeftAssociativeAST {

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
      }

      nodeNumber += 1
      var decListNode: ASTNode = new ASTNode(programNode, "decl list", null, nodeNumber, null, null)
      programNode.childrens += decListNode;

      declarations(decListNode)

      nodeNumber += 1
      var stmtListNode: ASTNode = new ASTNode(programNode, "stmt list", null, nodeNumber, null, null)
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
          } else throw new ASTError("Multiple declaration of the varriable '" + varName + "' at line " + remaining.head.lineNumber)
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
      nodeNumber += 1
      var expressionNode: ASTNode = new ASTNode(parent, "expression", remaining.head, nodeNumber, null, null)
      parent.childrens += expressionNode

      var simResult: ASTNode = simpleExpression(expressionNode)
      
      parent.childrens.remove(parent.childrens.length - 1)
      parent.childrens += simResult
      
      simResult.parent = parent
      
      expressionPrime(parent)
      return parent
    }

    def simpleExpression(parent: ASTNode): ASTNode = {
      nodeNumber += 1
      var simpleExpressionNode: ASTNode = new ASTNode(parent, "simpleExpression", remaining.head, nodeNumber, null, null)
      parent.childrens += simpleExpressionNode

      ///////////////////////////////////////////////////////////////////////////
      nodeNumber += 1
      var termNode: ASTNode = new ASTNode(simpleExpressionNode, "term", remaining.head, nodeNumber, null, null)
      simpleExpressionNode.childrens += termNode
      
      nodeNumber += 1
      var simpleExpressionPrimeNode: ASTNode = new ASTNode(simpleExpressionNode, "simpleExpressionPrime", remaining.head, nodeNumber, null, null)
      simpleExpressionNode.childrens += simpleExpressionPrimeNode
      ///////////////////////////////////////////////////////////////////////////

      term(simpleExpressionNode, termNode)
      simpleExpressionPrimeNode.st = termNode.ptr
      
      simpleExpressionPrime(simpleExpressionNode, simpleExpressionPrimeNode)
      simpleExpressionNode.ptr = simpleExpressionPrimeNode.ptr

      return simpleExpressionNode.ptr
    }

    def expressionPrime(parent: ASTNode): ASTNode = {
      if (matchTokenType(Constants.CompareText)) {
        nodeNumber += 1
        var compareNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)

        var simpleExpNode: ASTNode = parent.childrens.remove(parent.childrens.length - 1)
        simpleExpNode.parent = compareNode
        parent.childrens += compareNode

        compareNode.childrens += simpleExpNode
        consumeToken()
        return expression(compareNode)
      } else return null

    }

    def simpleExpressionPrime(parent: ASTNode, simpleExpressionPrimeNode_1: ASTNode): ASTNode = {

      if (matchTokenType(Constants.AdditiveText)) {
        nodeNumber += 1
        var binaryOpNode: ASTNode = new ASTNode(null, remaining.head.value, remaining.head, nodeNumber, null, null)
        consumeToken()
        
        nodeNumber += 1
        var termNode: ASTNode = new ASTNode(simpleExpressionPrimeNode_1, "term", remaining.head, nodeNumber, null, null)
        simpleExpressionPrimeNode_1.childrens += termNode
        
        nodeNumber += 1
        var simpleExpressionPrimeNode_2: ASTNode = new ASTNode(simpleExpressionPrimeNode_1, "simpleExpressionPrime", remaining.head, nodeNumber, null, null)
        simpleExpressionPrimeNode_1.childrens += simpleExpressionPrimeNode_2

        term(simpleExpressionPrimeNode_1, termNode)
        simpleExpressionPrimeNode_1.st.parent = binaryOpNode
        termNode.ptr.parent = binaryOpNode
        
        binaryOpNode.childrens += simpleExpressionPrimeNode_1.st
        binaryOpNode.childrens += termNode.ptr
        simpleExpressionPrimeNode_2.st = binaryOpNode
        
        simpleExpressionPrime(simpleExpressionPrimeNode_1, simpleExpressionPrimeNode_2)
        simpleExpressionPrimeNode_1.ptr = simpleExpressionPrimeNode_2.ptr
        parent
      } else {
        simpleExpressionPrimeNode_1.ptr = simpleExpressionPrimeNode_1.st
        return null
      }
    }

    def term(parent: ASTNode, termNode: ASTNode): ASTNode = {
      nodeNumber += 1
      var factorNode: ASTNode = new ASTNode(parent, "factor", remaining.head, nodeNumber, null, null)
      termNode.childrens += factorNode
      
      nodeNumber += 1
      var termPrimeNode: ASTNode = new ASTNode(parent, "termPrime", remaining.head, nodeNumber, null, null)
      termNode.childrens += termPrimeNode
      
      factor(termNode, factorNode)
      termPrimeNode.st = factorNode.ptr
      
      termPrime(termNode, termPrimeNode)
      termNode.ptr = termPrimeNode.ptr
      
      return parent
    }


    def termPrime(parent: ASTNode, termPrimeNode: ASTNode): ASTNode = {
      if (matchTokenType(Constants.MultiplicativeText)) {
        nodeNumber += 1
        var binaryOpNode: ASTNode = new ASTNode(null, remaining.head.value, remaining.head, nodeNumber, null, null)
        consumeToken()
        
        nodeNumber += 1
        var factorNode: ASTNode = new ASTNode(termPrimeNode, "factor", remaining.head, nodeNumber, null, null)
        termPrimeNode.childrens += factorNode
        
        nodeNumber += 1
        var simpleExpressionPrimeNode_2: ASTNode = new ASTNode(termPrimeNode, "termPrime", remaining.head, nodeNumber, null, null)
        termPrimeNode.childrens += simpleExpressionPrimeNode_2

        factor(termPrimeNode, factorNode)
        termPrimeNode.st.parent = binaryOpNode
        factorNode.ptr.parent = binaryOpNode
        
        binaryOpNode.childrens += termPrimeNode.st
        binaryOpNode.childrens += factorNode.ptr
        simpleExpressionPrimeNode_2.st = binaryOpNode
        
        termPrime(termPrimeNode, simpleExpressionPrimeNode_2)
        termPrimeNode.ptr = simpleExpressionPrimeNode_2.ptr
        parent
      } else {
        termPrimeNode.ptr = termPrimeNode.st
        return null
      }
    }

    /**
     * Create subtree for factor and return the head ASTNode
     */
    def factor(parent: ASTNode, factorNode: ASTNode): ASTNode = {
      if (matchTokenType(Constants.IdentText)) {
        var identTextNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
        factorNode.ptr = identTextNode
        consumeToken()
      } else if (matchTokenType(Constants.IntLitText)) {
        var intLitTextNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
        factorNode.ptr = intLitTextNode
        consumeToken()
      } else if (matchTokenType(Constants.BoolLitText)) {
        var boolLitTextNode: ASTNode = new ASTNode(parent, remaining.head.value, remaining.head, nodeNumber, null, null)
        factorNode.ptr = boolLitTextNode
        consumeToken()
      } else if (matchTokenTypeAndValue(Constants.OperatorText, "(")) {
        consumeToken()
        factorNode.ptr = expression(factorNode).childrens.head
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
      //System.out.println(n.nodeLabel + " : " + n.childrens.length)
      resultStr += s + "\n";

      count += 1
      for (x <- n.childrens) {
        //x.parent = n
        travarsalQ.enqueue(x)
      }
    }
    resultStr += "\n}"
    //System.out.println(resultStr)
    writeToFile(resultStr, astFileName)
    resultStr = ""
  }

}
