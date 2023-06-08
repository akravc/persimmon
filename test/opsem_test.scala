import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestParser._
import PrettyPrint._
import PersimmonWF._
import PersimmonUtil.*
import PersimmonReduction._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class OPSTesting extends AnyFunSuite {
    val prog = Sp(Prog)

    test("wf - ex: abcode_main") {
        val p = readFile("res/abcode_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(NExp(5))(normalize(List(Prog), PersimmonProgram.exp.get))
    }

    test("wf - ex: default_main") {
        val p = readFile("res/default_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(BExp(true))(normalize(List(Prog), PersimmonProgram.exp.get))
    }

    test("wf - ex: even_odd_main") {
        val p = readFile("res/even_odd_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(BExp(false))(normalize(List(Prog), PersimmonProgram.exp.get))
    }

    test("wf - ex: abcodepaper_main") {
        val p = readFile("res/abcodepaper_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(NExp(6))(normalize(List(Prog), PersimmonProgram.exp.get))
    }
}