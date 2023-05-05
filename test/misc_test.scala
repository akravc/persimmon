import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._ 
import PersimmonTyping._ 
import TestDefParser._
import TestTypParser._
import PrettyPrint._
import PersimmonWF._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File
import scala.io.Source

class MiscTesting extends AnyFunSuite {
  test("misc") {
    val prog = Source.fromFile("/Users/jonasiskander/persimmon/test/misc_test.txt").mkString
    assert(canParse(TestDefParser.pProgram, prog))
    val lkg = parseSuccess(TestDefParser.pProgram, prog)
    assert(wfDef(List(Prog), lkg))
  }
}
