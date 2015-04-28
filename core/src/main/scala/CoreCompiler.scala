/**
 * Authors: Joy, Sharif, Imtiaz, Tanvir
 */
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import java.util.StringTokenizer

case class SyntaxError(msg: String) extends Exception(msg)
case class ParseError(msg: String) extends Exception(msg)
case class ASTError(msg: String) extends Exception(msg)
case class TypeError(msg: String) extends Exception(msg)

object CoreCompiler {

  def main(args: Array[String]) {
    var arr = Array("tl-progs/simple2.tl", "tl-progs/simple1.tl", "tl-progs/sqrt.tl", "tl-progs/euclid.tl", "tl-progs/factorize.tl", "tl-progs/fibonacci.tl", "tl-progs/type-error1.tl")
    //var arr = Array("tl-progs/sqrt.tl")
    if (args.length == 0) {
      //      Console.err.println("No arguments.")
      //      sys.exit(0)
    } else {
      arr = args;
    }

    for (
      arg <- arr if !arg.endsWith(".tl")
    ) {
      Console.err.println("Input file name, $arg, doesn't end with \".tl\"")
      sys.exit(1)
    }

    for (
      arg <- arr;
      fileNameStem = arg.replaceFirst(".tl$", "")
    ) {
      try {
        val sourceFileName = fileNameStem + ".tl"
        val parseTreeName = fileNameStem + ".pt.dot"
        val astName = fileNameStem + ".ast.dot"
        val ilocCFGName = fileNameStem + ".A3.cfg.dot"
        val mipsAsmName = fileNameStem + ".s"

        val source = Source.fromFile(sourceFileName)

        // Replace this with your with calls to your parser, etc.

        var tokenList: ListBuffer[Token] = new Scanner().getTokenListFromFile(sourceFileName)
        var parser: Parser = new Parser()
        parser.getParseTree(tokenList)
        parser.buildGraphViz(parseTreeName)

        //var ast: AST_New = new AST_New()
        var ast: LeftAssociativeAST = new LeftAssociativeAST()
        var astRoot: ASTNode = ast.getAbstractSyntextTree(tokenList)
        ast.generateGraphVizOuptutFile(astName)

        var symbolTable = ast.symbolTable

        //symbolTable.keys.foreach { i => print( "Key = " + i )
        //                 println(" Value = " + symbolTable(i) )}
        var typeChecker: TypeChecker = new TypeChecker
        if (typeChecker.checkProgram(astRoot, symbolTable))
          println("Done")

        var iloc = new ILOC
        iloc.generateILOC(astRoot, symbolTable)
        iloc.buildGraphViz(ilocCFGName)
        /*var st: StringTokenizer = new StringTokenizer(iloc.fullILOCCOdes,"\n")
        while (st.hasMoreTokens()) {
          println(st.nextToken())
        }*/
        var mips= new MIPS
        mips.generateMIPS(iloc.fullILOCCOdes, mipsAsmName)

      } catch {
        case e: SyntaxError => {
          print(s"Syntax Error [$fileNameStem.tl]: ")
          println(e.getMessage())
        }
        case e: ParseError => {
          print(s"Error while parsing [$fileNameStem.tl]: ")
          println(e.getMessage())
        }
        case e: ASTError => {
          print(s"Error while generating AST [$fileNameStem.tl]: ")
          println(e.getMessage())
          //e.printStackTrace()
        }
        case e: TypeError => {
          print(s"Type mismatch in [$fileNameStem.tl]: ")
          println(e.getMessage())
          //e.printStackTrace()
        }
        case e: Exception => {
          print(s"Error processing [$fileNameStem.tl]: ")
          println(e.getMessage())
          e.printStackTrace()
        }
      }
    }
  }
}

