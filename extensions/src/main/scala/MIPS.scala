import java.util.StringTokenizer
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import java.io.File
/**
 * @author Sharif-MSI
 */
class MIPS {

  var fullMipsCodes = "\t.data\nnewline:  .asciiz \"\\n\"\n\t.text\n\t.globl main\nmain:\n\tli $fp, 0x7ffffffc\n"
  var framePointer = 0;
  var registerToStack = HashMap.empty[String, Int]
  var ILOCToMIPS = HashMap.empty[String, String]

  var extraLevelCounter: Int = 0
  def getNewExtraLevel(): String = {
    extraLevelCounter += 1
    return "L1" + extraLevelCounter
  }

  def generateMIPS(ILOCCode: String, fileName: String) {
    ILOCToMIPS += ("cmp_LE" -> "sle")
    ILOCToMIPS += ("cmp_GE" -> "sge")
    ILOCToMIPS += ("cmp_LT" -> "slt")
    ILOCToMIPS += ("cmp_GT" -> "sgt")
    ILOCToMIPS += ("cmp_EQ" -> "seq")
    ILOCToMIPS += ("cmp_NE" -> "sne")
    var st: StringTokenizer = new StringTokenizer(ILOCCode, "\n")
    while (st.hasMoreTokens()) {
      var codeLine = st.nextToken()
      //println(codeLine)
      mipsCodeForline(codeLine)

    }
    writeToFile(fullMipsCodes, fileName)
    //println(fullMipsCodes)
  }

  def mipsCodeForline(codeLine: String) {
    var opcodes = codeLine.split(" ")
    var operator = opcodes(0)
    try {
      if (operator.startsWith("B")) {
        fullMipsCodes += operator + "\n"
      } else if (operator.startsWith("cmp")) {
        fullMipsCodes += "\n\t#" + codeLine + "\n"
        arithmatic(ILOCToMIPS(operator), opcodes(1).substring(0, opcodes(1).length() - 1), opcodes(2), opcodes(4))
      } else {
        fullMipsCodes += "\n\t#" + codeLine + "\n"

        operator match {
          case "loadI" => {
            fullMipsCodes += "\tli $t0, " + opcodes(1) + "\n"
            push(opcodes(3))
          }
          case "readInt" => {
            fullMipsCodes += "\tli $v0, 5\n\tsyscall\n\tadd $t0, $v0, $zero\n"
            push(opcodes(2))

          }
          case "jumpI" => {
            fullMipsCodes += "\tj " + opcodes(2) + "\n"
          }
          case "i2i" => {
            fullMipsCodes += "\tlw $t1, " + registerToStack(opcodes(1)) + "($fp)\n\tadd $t0, $t1, $zero\n"
            push(opcodes(3))
          }
          case "add" | "sub" => {
            arithmatic(operator + "u", opcodes(1).substring(0, opcodes(1).length() - 1), opcodes(2), opcodes(4))
          }
          case "mult" => {
            arithmatic("mul", opcodes(1).substring(0, opcodes(1).length() - 1), opcodes(2), opcodes(4))
          }
          case "div" => {
            arithmatic(operator, opcodes(1).substring(0, opcodes(1).length() - 1), opcodes(2), opcodes(4))
          }
          case "mod" => {
            arithmatic("rem", opcodes(1).substring(0, opcodes(1).length() - 1), opcodes(2), opcodes(4))
          }
          case "cbr" => {
            fullMipsCodes += "\tlw $t0, " + registerToStack(opcodes(1)) + "($fp)\n\tbne $t0, $zero, "
            fullMipsCodes += opcodes(3).substring(0, opcodes(3).length() - 1) + "\n" + getNewExtraLevel() + ":\n"
            fullMipsCodes += "\tj " + opcodes(4) + "\n"

          }
          case "writeInt" => {
            fullMipsCodes += "\tli $v0, 1\n\tlw $t1, " + registerToStack(opcodes(1)) + "($fp)\n"
            fullMipsCodes += "\tadd $a0, $t1, $zero\n\tsyscall\n"
            fullMipsCodes += "\tli $v0, 4\n\tla $a0, newline\n\tsyscall\n"
          }
          case "exit" => {
            fullMipsCodes += "\tli $v0, 10\n\tsyscall\n"
          }
          case _ => {
            //println(operator)
          }
        }
      }
    } catch {
      case e: Exception => {
        println(operator + " has problems")
        e.printStackTrace()
      }
    }

  }

  def push(register: String) {
    var stackLocation = 0
    //  println("**"+register+"**")
    if (registerToStack.contains(register)) {
      //  println("**************************************************")
      stackLocation = registerToStack(register)
    } else {
      stackLocation = framePointer
      registerToStack += (register -> stackLocation)
      framePointer = framePointer - 4
    }
    fullMipsCodes += "\tsw $t0, " + stackLocation + "($fp)\n"
  }

  def arithmatic(operator: String, source1: String, source2: String, destination: String) {
    fullMipsCodes += "\tlw $t1, " + registerToStack(source1) + "($fp)\n"
    fullMipsCodes += "\tlw $t2, " + registerToStack(source2) + "($fp)\n"
    fullMipsCodes += "\t" + operator + " $t0, $t1, $t2\n"
    push(destination)
  }

  def writeToFile(fullMipsCodes: String, fileName: String) {
    val pw = new PrintWriter(new File(fileName))
    pw.write(fullMipsCodes)
    pw.close
  }
}