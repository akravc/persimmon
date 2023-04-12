import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestDefParser._
import TestTypParser._
import PrettyPrint._
import PersimmonWF._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class WFTesting extends AnyFunSuite {

    test("wf - collect all paths") {
        var fam = 
            """
            | Family A {
            |   Family K {
            |       Family C {
            |       }
            |   }
            |   Family M {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeTypLinkage(List(Prog), Sp(Prog))
        var p1 = SelfFamily(Sp(Prog), "A")
        var p2 = SelfFamily(Sp(p1), "K")
        var p3 = SelfFamily(Sp(p2), "C")
        var p4 = SelfFamily(Sp(p1), "M")
        assertResult{collectAllPathsWithin(lkg).toSet}{
            List(p1, p2, p3, p4, Prog).toSet
        }
    }

    test("wf - ancestors empty") {
        var fam = 
            """
            | Family A {
            |   Family K {
            |       Family C {
            |       }
            |   }
            |   Family M {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeTypLinkage(List(Prog), Sp(Prog))
        var p1 = SelfFamily(Sp(Prog), "A")
        var p2 = SelfFamily(Sp(p1), "K")
        var p3 = SelfFamily(Sp(p2), "C")
        var p4 = SelfFamily(Sp(p1), "M")
        assertResult{ancestors(p3).toSet}{
            List().toSet
        }
    }

    test("wf - ancestors nonempty") {
        var fam = 
            """
            | Family A {
            |   Family K {
            |       Family C extends M {
            |       }
            |   }
            |   Family M extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeTypLinkage(List(Prog), Sp(Prog))
        var p1 = SelfFamily(Sp(Prog), "A")
        var p2 = SelfFamily(Sp(p1), "K")
        var p3 = SelfFamily(Sp(p2), "C")
        var p4 = SelfFamily(Sp(p1), "M")
        assertResult{ancestors(p3).toSet}{
            List(p4, p1).toSet
        }
    }

    


}