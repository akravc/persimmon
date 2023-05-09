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

    /* ============= TEST PATH SUBSTITUTION ============= */

    test("linkage - path sub 1") {
        assertResult(
            subPathInPath(Sp(SelfFamily(Sp(Prog), "A")), 
                Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K")),
                Sp(SelfFamily(Sp(Prog), "A"))
            )
        ){
            Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K"))
        }
    }

    /* ============= TEST FRESH VARS ============= */

    test("linkage - bound vars 1") {
        var exp = Lam(Var("x"), BType, 
                    Lam(Var("y"), FunType(BType, NType), 
                        App(Var("y"), Var("x"))))
        assertResult(boundVarsInExp(exp)){List("x", "y")}
    }

    test("linkage - bound vars 2") {
        var exp = Lam(Var("x"), BType, 
                    Lam(Var("z"), FunType(BType, NType), 
                        App(Var("y"), Var("x"))))
        assertResult(boundVarsInExp(exp)){List("x", "z")}
    }

    test("linkage - fresh vars 1") {
        var exp = Lam(Var("x"), BType, 
                    Lam(Var("y"), FunType(BType, NType), 
                        App(Var("y"), Var("x"))))
        var bound = boundVarsInExp(exp)
        var fresh = freshVar(bound)
        assert(!bound.contains(fresh.id))
    }

    /* ============= TEST LINKAGE COMPUTATION: PROG ============= */

    test("linkage - prog lkg 1") {
        var fam = 
            """
            | Family A {
            |   Family K extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var p1 = Sp(Prog)
        var p2 = Sp(SelfFamily(p1, "A"))
        var p3 = Sp(SelfFamily(p2, "K"))
        assertResult(
            computeDefLinkage(List(), p1)
        ){
            DefinitionLinkage(
                p1, None, Map(), Map(), Map(), Map(), Map(),
                Map("A" -> 
                    DefinitionLinkage(
                        p2, None, Map(), Map(), Map(), Map(), Map(),
                        Map("K" -> DefinitionLinkage(
                            p3, Some(AbsoluteFamily(p1, "A")),
                            Map(), Map(), Map(), Map(), Map(), Map())))))
        }
    }

    test("linkage - prog lkg 2") {
        var fam = 
            """
            | Family A1 {
            |   Family B1 {
            |   }
            |   Family B2 extends self(A1).B1 {
            |   }
            | }
            |
            | Family A2 extends A1 {
            |   Family B1 {
            |   }
            |   Family B2 extends self(A2).B1 {
            |   }
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var p = Sp(Prog)
        var a1 = Sp(SelfFamily(p, "A1"))
        var b1 = Sp(SelfFamily(a1, "B1"))
        var b2 = Sp(SelfFamily(a1, "B2"))
        var a2 = Sp(SelfFamily(p, "A2"))
        var a2b1 = Sp(SelfFamily(a2, "B1"))
        var a2b2 = Sp(SelfFamily(a2, "B2"))
        assertResult(
            computeDefLinkage(List(), p)
        ){
            DefinitionLinkage(p, None, Map(), Map(), Map(), Map(), Map(),
                Map("A1" -> DefinitionLinkage(a1, None, Map(), Map(), Map(), Map(), Map(), 
                Map("B1" -> DefinitionLinkage(b1, None, Map(), Map(), Map(), Map(), Map(), Map()), 
                "B2" -> DefinitionLinkage(b2, Some(AbsoluteFamily(a1, "B1")), Map(), Map(), Map(), Map(), Map(), Map()))), 
                "A2" -> DefinitionLinkage(a2, Some(AbsoluteFamily(p, "A1")), Map(), Map(), Map(), Map(), Map(), 
                Map("B1" -> DefinitionLinkage(a2b1, None, Map(), Map(), Map(), Map(), Map(), Map()), 
                "B2" -> DefinitionLinkage(a2b2, Some(AbsoluteFamily(a2, "B1")), Map(), Map(), Map(), Map(), Map(), Map())))))
        }
    }



    /* ============= TEST LINKAGE COMPUTATION: NESTED ============= */

    test("linkage - Reviewer compute example") {
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

    test("linkage - Reviewer compute example2") {
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

    
    /* ============= TEST FURTHER BINDING ============= */

    test("linkage - TODO: futher binding test") {
        var fam = 
            """
            | Family A1 {
            |   Family B1 {
            |   }
            |   Family B2 extends self(A1).B1 {
            |   }
            | }
            |
            | Family A2 extends A1 {
            |   Family B1 {
            |   }
            |   Family B2 extends self(A2).B1 {
            |   }
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var p = Sp(Prog)
        var a1 = Sp(SelfFamily(p, "A1"))
        var b1 = Sp(SelfFamily(a1, "B1"))
        var b2 = Sp(SelfFamily(a1, "B2"))
        var a2 = Sp(SelfFamily(p, "A2"))
        var a2b1 = Sp(SelfFamily(a2, "B1"))
        var a2b2 = Sp(SelfFamily(a2, "B2"))
        assertResult(
            computeDefLinkage(List(), p)
        ){
            DefinitionLinkage(p, None, Map(), Map(), Map(), Map(), Map(),
                Map("A1" -> DefinitionLinkage(a1, None, Map(), Map(), Map(), Map(), Map(), 
                Map("B1" -> DefinitionLinkage(b1, None, Map(), Map(), Map(), Map(), Map(), Map()), 
                "B2" -> DefinitionLinkage(b2, Some(AbsoluteFamily(a1, "B1")), Map(), Map(), Map(), Map(), Map(), Map()))), 
                "A2" -> DefinitionLinkage(a2, Some(AbsoluteFamily(p, "A1")), Map(), Map(), Map(), Map(), Map(), 
                Map("B1" -> DefinitionLinkage(a2b1, None, Map(), Map(), Map(), Map(), Map(), Map()), 
                "B2" -> DefinitionLinkage(a2b2, Some(AbsoluteFamily(a2, "B1")), Map(), Map(), Map(), Map(), Map(), Map())))))
        }
    }

}