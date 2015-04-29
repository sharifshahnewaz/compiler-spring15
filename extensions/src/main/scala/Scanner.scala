/*
 * The scanner.scala file will be called with file_name to lexically analyze
 * This module will populate the token list based on the regular patterns
 * */
import java.util.StringTokenizer
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

/**
 * @author Joy Rahman, Sharif Shahnewaz, Tanvir Irfan Chowdhury, Imtiaz Muhammad
 */

class Scanner {

  var tokenListInFile = new ListBuffer[Token]

  /**
   * param    : one string line
   * returns  : no return value
   * comment  : populate "tokenListInFile" using the Token Found
   */

  def getTokensFromLine(line: String, lineNumber: Int) {
    var beginIndex: Int = 0;
    var endIndex: Int = if (line.contains("%")) {
      line.indexOf('%')
    } else {
      line.length()
    }
    var codeLine: String = line.substring(beginIndex, endIndex).trim()
    var st: StringTokenizer = new StringTokenizer(codeLine)

    while (st.hasMoreTokens()) {
      tokenListInFile += findTokenPattern(st.nextToken(), lineNumber)
    }

  }

  /**
   * param    : one string word
   * returns  : new Token of type
   * comment  : throws the Exception if can not match with the rule
   */

  def findTokenPattern(item: String, lineNumber: Int): Token = {
    item match {

      case "(" | ")" | ":=" | ";" | ":" | "," => {
        return new Token(item, Constants.OperatorText, lineNumber);
      }
      case "*" | "div" | "mod" => {
        return new Token(item, Constants.MultiplicativeText, lineNumber);
      }
      case "+" | "-" => {
        return new Token(item, Constants.AdditiveText, lineNumber);
      }
      case "=" | "!=" | "<" | ">" | "<=" | ">=" => {
        return new Token(item, Constants.CompareText, lineNumber);
      }
      case Constants.INTLIT() => {
        return new Token(item, Constants.IntLitText, lineNumber);
      }
      case Constants.BOOLLIT() => {
        return new Token(item, Constants.BoolLitText, lineNumber);
      }
      case Constants.IDENT() => {
        return new Token(item, Constants.IdentText, lineNumber);
      }
      case Constants.KEYWORD() => {
        return new Token(item, Constants.KeywordText, lineNumber);
      }
      case Constants.PROCEDURE() => {
        return new Token(item, Constants.ProcedureText, lineNumber);
      }
      case _ => {
        throw new SyntaxError("Token : " + item + " at line " + lineNumber + " does not match with any valid token type.")
        return null
      }
    }
  }

  /**
   * param    : File name that needs to be tokenized
   * returns  : List[Token]
   * comment  : populate "tokenListInFile" using the Token Found
   */
  def getTokenListFromFile(sourceFileName: String): ListBuffer[Token] = {
    val source = Source.fromFile(sourceFileName)
    var lineNumber: Int = 1
    Console.println(s"\n *** $sourceFileName")
    for (line <- source.getLines) {
      getTokensFromLine(line, lineNumber)
      lineNumber += 1
    }

    return tokenListInFile
  }

  def printList(abc: ListBuffer[Token]) {
    for (x <- abc)
      println(x.value + " of type " + x.tokenType)

  }

}