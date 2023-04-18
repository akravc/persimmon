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

class LinkageTesting extends AnyFunSuite {

    /* ============= TEST LINKAGE COMPUTATION ============= */

    test("Reviewer compute example") {
        var fam = 
            """
            | Family A {
            |   Family K extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        var p1 = SelfFamily(Sp(Prog), "A")
        var p2 = SelfFamily(Sp(p1), "K")
        PersimmonLinkages.p = fam
        assertResult(
        computeDefLinkage(List(p1, p2), Sp(p2))
        ){
        DefinitionLinkage(
            SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K"), 
            Some(AbsoluteFamily(Sp(Prog), "A")), 
            Map(), Map(), Map(), Map(), Map(), 
            Map("K" -> DefinitionLinkage(
                SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K"), 
                Some(AbsoluteFamily(Sp(Prog), "A")), 
                Map(), Map(), Map(), Map(), Map(), Map())))
        }
    }
}