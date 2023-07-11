import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._ 
import PersimmonTyping._ 
import TestParser._
import PrettyPrint._
import PersimmonWF._
import PersimmonUtil.*
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class LinkageTesting extends AnyFunSuite {

    test("linkage - extension alternating self-paths") {
        val fam = 
            """
            | Family A1 {
            |   Family B1 {
            |       type X = C1 {}
            |
            |       val foo: X -> N = lam(x: X). 1
            |   }
            |   Family B2 extends self(A1).B1 {
            |   }
            |   Family B3 extends A1.B1 {
            |   }
            | }
            |
            | Family A2 extends A1 {
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val p = Sp(Prog)
        val a1 = Sp(SelfFamily(p, "A1"))
        val b1 = Sp(SelfFamily(a1, "B1"))
        val b2 = Sp(SelfFamily(a1, "B2"))
        val b3 = Sp(SelfFamily(a1, "B3"))

    }

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
        val exp = Lam(Var("x"), BType, 
                    Lam(Var("y"), FunType(BType, NType), 
                        App(Var("y"), Var("x"))))
        assertResult(boundVarsInExp(exp)){Set("x", "y")}
    }

    test("linkage - bound vars 2") {
        val exp = Lam(Var("x"), BType, 
                    Lam(Var("z"), FunType(BType, NType), 
                        App(Var("y"), Var("x"))))
        assertResult(boundVarsInExp(exp)){Set("x", "z")}
    }

    test("linkage - fresh vars 1") {
        val exp = Lam(Var("x"), BType, 
                    Lam(Var("y"), FunType(BType, NType), 
                        App(Var("y"), Var("x"))))
        val bound = boundVarsInExp(exp)
        val fresh = freshVar(bound)
        assert(!bound.contains(fresh.id))
    }

    /* ============= TEST LINKAGE COMPUTATION: PROG ============= */

    test("linkage - prog lkg 1") {
        val fam = 
            """
            | Family A {
            |   Family K extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val p1 = Sp(Prog)
        val p2 = Sp(SelfFamily(p1, "A"))
        val p3 = Sp(SelfFamily(p2, "K"))
        assertResult(
            computeDefLinkage(p1)
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
        val fam = 
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
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val p = Sp(Prog)
        val a1 = Sp(SelfFamily(p, "A1"))
        val b1 = Sp(SelfFamily(a1, "B1"))
        val b2 = Sp(SelfFamily(a1, "B2"))
        val a2 = Sp(SelfFamily(p, "A2"))
        val a2b1 = Sp(SelfFamily(a2, "B1"))
        val a2b2 = Sp(SelfFamily(a2, "B2"))
        assertResult(
            computeDefLinkage(p)
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
        val fam = 
            """
            | Family A {
            |   Family K extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        val p1 = SelfFamily(Sp(Prog), "A")
        val p2 = SelfFamily(Sp(p1), "K")
        PersimmonLinkages.p = fam
        assertResult(
        computeDefLinkage(Sp(p2))
        ){
        DefinitionLinkage(
            Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K")), 
            Some(AbsoluteFamily(Sp(Prog), "A")), Map(), Map(), Map(), Map(), Map(), 
            Map("K" -> DefinitionLinkage(
                Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "K")), "K")), 
                Some(AbsoluteFamily(Sp(Prog), "A")), Map(), Map(), Map(), Map(), Map(), Map())))
        }
    }

    
    /* ============= TEST FURTHER BINDING ============= */

    test("linkage - futher binding test") {
        val fam = 
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
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val p = Sp(Prog)
        val a1 = Sp(SelfFamily(p, "A1"))
        val b1 = Sp(SelfFamily(a1, "B1"))
        val b2 = Sp(SelfFamily(a1, "B2"))
        val a2 = Sp(SelfFamily(p, "A2"))
        val a2b1 = Sp(SelfFamily(a2, "B1"))
        val a2b2 = Sp(SelfFamily(a2, "B2"))
        assertResult(
            computeDefLinkage(p)
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
