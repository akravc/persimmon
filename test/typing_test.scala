import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._ 
import TestDefParser._
import TestTypParser._
import PrettyPrint._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class TypecheckerTesting extends AnyFunSuite {

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
        // assertResult(
        // computeDefLinkage(List(p1, p2), Sp(p2))
        // ){
        // DefinitionLinkage(Sp(p2), p2, Some(p1), 
        //     Map(), Map(), Map(), Map(), Map(), Map())
        // }
    }

}