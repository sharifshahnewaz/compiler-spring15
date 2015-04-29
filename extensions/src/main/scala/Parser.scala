import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File

/**
 * File Name    :   Parser.scala
 * Author       :   Joy Rahman, Sharif Shahnewaz, Tanvir Irfan, Imtiaz Muhammad
 * Purpose      :   Parse tokens and build parse trees
 */

class Parser {

  var nodeListInFile = new ListBuffer[Node]
  var nodeNumber: Int = 0

  def getParseTree(tokenList: ListBuffer[Token]): Node = {
    var remaining = tokenList //remaining list of the tokenlist
    nodeNumber += 1
    var programNode = new Node(null, "program", null, nodeNumber) //this is the dummy header for the whole tree
    nodeListInFile += programNode
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
    def addChildrenAndConsumeTokenExtMessage(currentNode: Node, extraValueString: String) {
      nodeNumber += 1
      var n: Node = new Node(currentNode, extraValueString + "" + remaining.head.value, remaining.head, nodeNumber)
      currentNode.childrens += (n)
      nodeListInFile += (n)
      remaining = remaining.tail
    }
    /**
     * consume first token of the list and add token as a leaf of the tree
     */
    def addChildrenAndConsumeToken(currentNode: Node) {
      //System.out.println(remaining.head.value.toString())
      if (matchTokenType(Constants.IdentText)) {
        addChildrenAndConsumeTokenExtMessage(currentNode, "ident: ")
      } else if (matchTokenType(Constants.IntLitText)) {
        addChildrenAndConsumeTokenExtMessage(currentNode, "num: ")
      } else addChildrenAndConsumeTokenExtMessage(currentNode, "")

    }
    /**
     * crate a child node for the terminals
     */
    def getNodeAndConsumeToken(parent: Node): Node = {
      nodeNumber += 1
      var childNode = new Node(parent, remaining.head.value, remaining.head, nodeNumber)
      nodeListInFile += (childNode)
      remaining = remaining.tail
      return childNode
    }
    /**
     * Match a token type and value with first value token of token list
     */
    def matchTokenTypeAndValue(tokenType: String, tokenValue: String): Boolean = {
      if (!matchTokenType(tokenType)) throw new ParseError("Expected " + tokenType + ", found " + remaining.head.tokenType + " at line " + remaining.head.lineNumber)
      if (!matchTokenValue(tokenValue)) throw new ParseError("Expected " + tokenType + ": " + tokenValue + ", found " + tokenType + ": " + remaining.head.value + " at line " + remaining.head.lineNumber)
      return true
    }

    /**
     * consume first token of the list
     */

    def consumeToken() {
      remaining = remaining.tail
    }

    def epsilonNode(parent: Node): Node = {
      nodeNumber += 1
      var n: Node = new Node(parent, "ε", null, nodeNumber)
      nodeListInFile += (n)
      return n
    }

    /**
     * Starting point of the program this creates and returns the dummy head node
     */

    def program(): Node = {

      if (matchTokenTypeAndValue(Constants.KeywordText, "program")) {
        addChildrenAndConsumeToken(programNode)
      }

      programNode.childrens += declarations(programNode) // add the subtree got from declarations as second child
      //if (remaining.head.value.equals("procedure")) {
      programNode.childrens += procedures(programNode)
      //}
      if (matchTokenTypeAndValue(Constants.KeywordText, "begin")) {
        addChildrenAndConsumeToken(programNode)
      }

      programNode.childrens += statementSequence(programNode)

      if (matchTokenTypeAndValue(Constants.KeywordText, "end")) {
        addChildrenAndConsumeToken(programNode)
      }

      return programNode
    }

    def parameters(parent: Node): Node = {
      nodeNumber += 1
      var parametersNode: Node = new Node(parent, "parameters", null, nodeNumber)
      nodeListInFile += parametersNode
      if (!remaining.head.value.equals(")")) {
        if (matchTokenType(Constants.IdentText)) {
          addChildrenAndConsumeToken(parametersNode)
        }
        if (matchTokenTypeAndValue(Constants.OperatorText, ":")) {
          addChildrenAndConsumeToken(parametersNode)
        }
        parametersNode.childrens += typeNT(parametersNode)
        
        if (remaining.head.value.equals(";")) {
          addChildrenAndConsumeToken(parametersNode)          
        }
        parametersNode.childrens += parameters(parametersNode)
      } else parametersNode.childrens += epsilonNode(parametersNode)
      return parametersNode
    }

    def procedures(parent: Node): Node = {
      nodeNumber += 1
      var proceduresNode: Node = new Node(parent, "procedures", null, nodeNumber)
      nodeListInFile += proceduresNode

      if (!remaining.head.value.equals("begin")) {
        if (matchTokenTypeAndValue(Constants.KeywordText, "procedure")) {
          addChildrenAndConsumeToken(proceduresNode)
        }
        if (matchTokenType(Constants.IdentText)) {
          addChildrenAndConsumeToken(proceduresNode)
        }
        if (matchTokenTypeAndValue(Constants.OperatorText, "(")) {
          addChildrenAndConsumeToken(proceduresNode)
        }

        proceduresNode.childrens += parameters(proceduresNode)

        if (matchTokenTypeAndValue(Constants.OperatorText, ")")) {
          addChildrenAndConsumeToken(proceduresNode)
        }
        if (matchTokenTypeAndValue(Constants.OperatorText, ";")) {
          addChildrenAndConsumeToken(proceduresNode)
        }

        proceduresNode.childrens += declarations(proceduresNode)

        if (matchTokenTypeAndValue(Constants.KeywordText, "begin")) {
          addChildrenAndConsumeToken(proceduresNode)
        }

        proceduresNode.childrens += statementSequence(proceduresNode)

        if (matchTokenTypeAndValue(Constants.KeywordText, "end")) {
          addChildrenAndConsumeToken(proceduresNode)
        }
        if (matchTokenTypeAndValue(Constants.OperatorText, ";")) {
          addChildrenAndConsumeToken(proceduresNode)
        }
        proceduresNode.childrens += procedures(proceduresNode)
      } else proceduresNode.childrens += epsilonNode(proceduresNode)

      return proceduresNode
    }

    /**
     *      declarations()
     *      Recursively create subtree for declarations and return the head node
     *      <declarations> ::= VAR ident AS <type> SC <declarations> | ε
     */

    def declarations(parent: Node): Node = {
      nodeNumber += 1
      var declarationsNode: Node = new Node(parent, "declarations", null, nodeNumber)
      nodeListInFile += (declarationsNode)
      if ((!remaining.head.value.equals("begin")) && (!remaining.head.value.equals("procedure"))) {
        if (matchTokenTypeAndValue(Constants.KeywordText, "var")) {
          addChildrenAndConsumeToken(declarationsNode)
        }
        if (matchTokenType(Constants.IdentText)) {
          addChildrenAndConsumeToken(declarationsNode)

        } else {
          throw new ParseError("Expected Identifier , found" + remaining.head.tokenType + " at line " + remaining.head.lineNumber)
        }
        if (matchTokenTypeAndValue(Constants.KeywordText, "as")) {
          addChildrenAndConsumeToken(declarationsNode)
        }

        declarationsNode.childrens += typeNT(declarationsNode)

        if (matchTokenTypeAndValue(Constants.OperatorText, ";")) {
          addChildrenAndConsumeToken(declarationsNode)
        }

        declarationsNode.childrens += declarations(declarationsNode)
      } else declarationsNode.childrens += epsilonNode(declarationsNode)

      return declarationsNode
    }

    /**
     * Create subtree for type non terminal and return the head node
     */
    def typeNT(parent: Node): Node = {
      nodeNumber += 1
      var typeNode: Node = new Node(parent, "type", null, nodeNumber)
      nodeListInFile += (typeNode)
      if (matchTokenType(Constants.KeywordText)) {
        if (matchTokenValue("int") || matchTokenValue("bool")) {
          addChildrenAndConsumeToken(typeNode)
        } else throw new ParseError("Expected Keyword: int or bool , found Keyword: " + remaining.head.value + " at line " + remaining.head.lineNumber)

      } else throw new ParseError("Expected Keyword , found" + remaining.head.tokenType + " at line " + remaining.head.lineNumber)

      return typeNode;
    }

    /**
     * Create subtree for statementSequence and return the head node
     */
    def statementSequence(parent: Node): Node = {

      nodeNumber += 1
      var statementSeqNode: Node = new Node(parent, "statementSequence", null, nodeNumber)
      nodeListInFile += (statementSeqNode)
      if (remaining.head.value.equals("end") || remaining.head.value.equals("else")) {
        statementSeqNode.childrens += epsilonNode(statementSeqNode)
      } else {
        statementSeqNode.childrens += statement(statementSeqNode);
        if (matchTokenTypeAndValue(Constants.OperatorText, ";")) {
          addChildrenAndConsumeToken(statementSeqNode)
        }
        statementSeqNode.childrens += statementSequence(statementSeqNode)
      }
      return statementSeqNode
    }

    /**
     * Create subtree for statement and return the head node
     */
    def statement(parent: Node): Node = {
      nodeNumber += 1
      var statementNode: Node = new Node(parent, "statement", null, nodeNumber)
      nodeListInFile += (statementNode)
      if (matchTokenType(Constants.IdentText)) {
        statementNode.childrens += assignment(statementNode)
      } else if (matchTokenValue("if")) {
        statementNode.childrens += ifStatement(statementNode)
      } else if (matchTokenValue("while")) {
        statementNode.childrens += whileStatement(statementNode)
      } else if (matchTokenValue("writeInt")) {
        statementNode.childrens += writeInt(statementNode)
      }

      return statementNode
    }

    /**
     * Create subtree for assignment and return the head node
     */
    def assignment(parent: Node): Node = {
      nodeNumber += 1
      var assignmentNode: Node = new Node(parent, "assignment", null, nodeNumber)
      nodeListInFile += (assignmentNode)
      if (matchTokenType(Constants.IdentText)) {
        addChildrenAndConsumeToken(assignmentNode)
      } else throw new ParseError("Expected Identifier , found" + remaining.head.tokenType + " at line " + remaining.head.lineNumber)
      if (matchTokenTypeAndValue(Constants.OperatorText, ":=")) {
        addChildrenAndConsumeToken(assignmentNode)
      }
      assignmentNode.childrens += assignmentPrime(assignmentNode)
      return assignmentNode
    }

    /**
     * Create subtree for assignmentPrime and return the head node
     */
    def assignmentPrime(parent: Node): Node = {
      if (matchTokenType(Constants.ProcedureText) && matchTokenValue("readInt")) {
        return getNodeAndConsumeToken(parent)
      } else
        return expression(parent)
    }

    /**
     * Create subtree for ifStatement and return the head node
     */
    def ifStatement(parent: Node): Node = {
      nodeNumber += 1
      var ifNode: Node = new Node(parent, "ifStatement", null, nodeNumber)
      nodeListInFile += (ifNode)
      if (matchTokenTypeAndValue(Constants.KeywordText, "if")) {
        addChildrenAndConsumeToken(ifNode)
      }
      ifNode.childrens += expression(ifNode);
      if (matchTokenTypeAndValue(Constants.KeywordText, "then")) {
        addChildrenAndConsumeToken(ifNode)
      }
      ifNode.childrens += statementSequence(ifNode);
      ifNode.childrens += elseClause(ifNode)
      if (matchTokenTypeAndValue(Constants.KeywordText, "end")) {
        addChildrenAndConsumeToken(ifNode)
      }
      return ifNode;
    }

    /**
     * Create subtree for elseClause and return the head node
     */
    def elseClause(parent: Node): Node = {
      nodeNumber += 1
      var elseNode: Node = new Node(parent, "elseClause", null, nodeNumber)
      nodeListInFile += (elseNode)
      if (matchTokenValue("else")) {
        if (matchTokenTypeAndValue(Constants.KeywordText, "else")) {
          addChildrenAndConsumeToken(elseNode)
        }
        elseNode.childrens += statementSequence(elseNode)
      } else elseNode.childrens += epsilonNode(elseNode)
      return elseNode
    }

    /**
     * Create subtree for whileStatement and return the head node
     */
    def whileStatement(parent: Node): Node = {
      nodeNumber += 1
      var whileNode: Node = new Node(parent, "whileStatement", null, nodeNumber)
      nodeListInFile += (whileNode)
      if (matchTokenTypeAndValue(Constants.KeywordText, "while")) {
        addChildrenAndConsumeToken(whileNode)
      }
      whileNode.childrens += expression(whileNode);
      if (matchTokenTypeAndValue(Constants.KeywordText, "do")) {
        addChildrenAndConsumeToken(whileNode)
      }
      whileNode.childrens += statementSequence(whileNode);

      if (matchTokenTypeAndValue(Constants.KeywordText, "end")) {
        addChildrenAndConsumeToken(whileNode)
      }
      return whileNode;
    }

    /**
     * Create subtree for writeInt and return the head node
     */
    def writeInt(parent: Node): Node = {
      nodeNumber += 1
      var writeNode: Node = new Node(parent, "writeInt", null, nodeNumber)
      nodeListInFile += (writeNode)
      if (matchTokenTypeAndValue(Constants.ProcedureText, "writeInt")) {
        addChildrenAndConsumeToken(writeNode)
      }
      writeNode.childrens += expression(writeNode);
      return writeNode;
    }

    /**
     * Create subtree for expression and return the head node
     */
    def expression(parent: Node): Node = {
      nodeNumber += 1
      var expressionNode: Node = new Node(parent, "expression", null, nodeNumber)
      nodeListInFile += (expressionNode)
      expressionNode.childrens += simpleExpression(expressionNode)
      expressionNode.childrens += expressionPrime(expressionNode)
      return expressionNode
    }

    /**
     * Create subtree for expressionPrime and return the head node
     */
    def expressionPrime(parent: Node): Node = {

      if (matchTokenType(Constants.CompareText)) {
        addChildrenAndConsumeToken(parent)
        return expression(parent)
      } else return null

    }

    /**
     * Create subtree for simple expression and return the head node
     */
    def simpleExpression(parent: Node): Node = {
      nodeNumber += 1
      var simpEexprNode: Node = new Node(parent, "simpleExpression", null, nodeNumber)
      nodeListInFile += (simpEexprNode)
      simpEexprNode.childrens += term(simpEexprNode)
      simpEexprNode.childrens += simpleExpressionPrime(simpEexprNode)
      return simpEexprNode
    }

    /**
     * Create subtree for simple expressionPrime and return the head node
     */
    def simpleExpressionPrime(parent: Node): Node = {
      if (matchTokenType(Constants.AdditiveText)) {
        addChildrenAndConsumeToken(parent)
        return simpleExpression(parent)
      } else return null
    }

    /**
     * Create subtree for term and return the head node
     */
    def term(parent: Node): Node = {
      nodeNumber += 1
      var termNode: Node = new Node(parent, "term", null, nodeNumber)
      nodeListInFile += (termNode)
      termNode.childrens += factor(termNode)
      termNode.childrens += termPrime(termNode)
      return termNode
    }

    /**
     * Create subtree for term prime and return the head node
     */
    def termPrime(parent: Node): Node = {
      if (matchTokenType(Constants.MultiplicativeText)) {
        addChildrenAndConsumeToken(parent)
        return term(parent)
      } else return return null
    }

    /**
     * Create subtree for factor and return the head node
     */
    def factor(parent: Node): Node = {
      nodeNumber += 1
      var factorNode: Node = new Node(parent, "factor", null, nodeNumber)
      nodeListInFile += (factorNode)
      if (matchTokenType(Constants.IdentText)) {
        addChildrenAndConsumeToken(factorNode)
      } else if (matchTokenType(Constants.IntLitText)) {
        addChildrenAndConsumeToken(factorNode)
      } else if (matchTokenType(Constants.BoolLitText)) {
        addChildrenAndConsumeToken(factorNode)
      } else if (matchTokenTypeAndValue(Constants.OperatorText, "(")) {
        addChildrenAndConsumeToken(factorNode)
        factorNode.childrens += expression(factorNode)
        if (matchTokenTypeAndValue(Constants.OperatorText, ")"))
          addChildrenAndConsumeToken(factorNode)
      }
      return factorNode;
    }

    def constractLineForGraphViz(parent: String, child: String, label: String): String = {
      var str: String = child + " [label=\"" + label + "\",fillcolor=\"/x11/white\",shape=box]\n" + parent + " -> " + child
      return str;
    }

    //calling the program function as a start point to create the parse tree
    program()
  }

  def buildGraphViz(parseTreeFileName: String) = {
    def constractLineForGraphViz(parent: String, child: String, label: String): String = {
      var str: String = child + " [label=\"" + label + "\",fillcolor=\"/x11/white\",shape=box]"
      if (parent.length() > 0) {
        str += "\n" + parent + " -> " + child
      }
      return str;
    }

    var resultStr: String = "digraph parseTree {\nordering=out;\nnode [shape = box, style = filled];\n"
    for (x <- nodeListInFile) {
      var s: String = ""
      if (x.parent != null) {
        s += constractLineForGraphViz(x.parent.nodeNumber + "", x.nodeNumber + "", x.nodeLabel)
      } else {
        s += constractLineForGraphViz("", x.nodeNumber + "", x.nodeLabel)
      }
      resultStr += s + "\n";
    }
    resultStr += "\n}"
    //    println(resultStr)
    writeToFile(resultStr, parseTreeFileName)
    resultStr = ""
    nodeListInFile.clear()
  }

  def writeToFile(graphViz: String, fileName: String) {
    val pw = new PrintWriter(new File(fileName))
    pw.write(graphViz)
    pw.close
  }

}
