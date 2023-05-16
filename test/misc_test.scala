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
  test("misc") {
    val prog = Source.fromFile("test/misc_test.txt").mkString
    assert(canParse(TestParser.pProgram, prog))
    val linkage = TestParser.parseProgramDefLink(prog)
    assert(wfDef(List(), linkage))
  }
}
