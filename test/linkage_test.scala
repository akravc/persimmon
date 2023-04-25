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

    test("path sub 1") {
        assertResult(
            subInPath(Sp(SelfFamily(Sp(Prog), "A")), 
                Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K")),
                Sp(SelfFamily(Sp(Prog), "A"))
            )
        ){
            Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K"))
        }
    }

    /* ============= TEST LINKAGE COMPUTATION ============= */

    test("prog lkg") {
        var fam = 
            """
            | Family A {
            |   Family K extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        printLkg(computeDefLinkage(List(), Sp(Prog)), "")
    }

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
            Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K")), 
            Some(AbsoluteFamily(Sp(Prog), "A")), Map(), Map(), Map(), Map(), Map(), 
            Map("K" -> DefinitionLinkage(
                Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K")), "K")), 
                Some(AbsoluteFamily(Sp(Prog), "A")), Map(), Map(), Map(), Map(), Map(), Map())))
        }
    }

    test("Reviewer compute example2") {
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
        var p3 = SelfFamily(Sp(p2), "K")
        var p4 = SelfFamily(Sp(p3), "K")
        PersimmonLinkages.p = fam
        // assertResult(
        // printLkg(computeDefLinkage(List(p1, p2, p3, p4), Sp(p4)), "")
        // ){
        // DefinitionLinkage(
        //     SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K"), 
        //     Some(AbsoluteFamily(Sp(Prog), "A")), 
        //     Map(), Map(), Map(), Map(), Map(), 
        //     Map("K" -> DefinitionLinkage(
        //         SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K"), 
        //         Some(AbsoluteFamily(Sp(Prog), "A")), 
        //         Map(), Map(), Map(), Map(), Map(), Map())))
        // }
    }
}