import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._ 
import PersimmonTyping._ 
import TestParser._
import PrettyPrint._
import PersimmonWF._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File
import scala.io.Source

class MiscTesting extends AnyFunSuite {

  def readFile(filename: String): String = { 
    return Source.fromFile(filename).getLines.mkString
  }

  test("misc") {
    val fam = readFile("res/misc")
    // must save the program in this global variable
    PersimmonLinkages.p = fam
    assert(canParse(TestParser.pProgram, fam))
    val linkage = TestParser.parseProgramDefLink(fam)
    assert(wfDef(List(Prog), linkage))
  }
}
