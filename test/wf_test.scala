import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestParser._
import PrettyPrint._
import PersimmonWF._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class WFTesting extends AnyFunSuite {
    var prog = Sp(Prog)

    /*========================== SMALL EXAMPLES ==========================*/



    test("wf - basic prog lkg") {
        var fam = 
            """
            | Family A {
            |
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - type") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - type - duplicate constructors") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} | C1 {}
            |}
            """.stripMargin
        // duplicate constructor names, error thrown while parsing
        assertThrows[Exception](canParse(TestParser.pProgram, fam))
    }

    test("wf - type - duplicate fields") {
        var fam = 
            """
            | Family A {
            |   type T = {a: N, a: B}
            |}
            """.stripMargin
        // duplicate field names, error thrown while parsing
        assertThrows[Exception](canParse(TestParser.pProgram, fam))
    }

    test("wf - type - duplicate type names") {
        var fam = 
            """
            | Family A {
            |   type T = {a: N, b: B}
            |   type T = C1 {}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ false }
    }

    test("wf - function") {
        var fam = 
            """
            | Family A {
            |   val g: N -> B = lam (x: N). true
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), Sp(Prog))
        var prog = Sp(Prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - function call") {
        var fam = 
            """
            | Family A {
            |   val goo: N -> B = lam (x: N). true
            |   val foo: N -> B = lam (y: N). (goo y)
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), Sp(Prog))
        var prog = Sp(Prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - cases") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   val f: T -> N = lam (x: T). match x with (c {})
            |   cases c <T> : {} -> {C1: {} -> N, C2: {} -> N} =
            |       lam (x: {}). 
            |           {C1 = lam (_: {}). 1, C2 = lam (_:{}). 2 }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - cases sugar") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   def f: T -> N = 
            |       case C1() = 1
            |       case C2() = 2
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - extend type") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {a: B} | C2 {n: N}
            | }
            |
            | Family C extends A {
            |   type T += C3 {a: B, n: N}
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }


    /*===================== LARGE & PAPER EXAMPLES =====================*/

    // replaced strings with ints because we don't have strings right now
    // included built in optionval type
    test("wf - STLCBase and extension from the paper") {
        var fam = 
            """
              | Family STLCBase {
              |   type Ty = TUnit {} | TNat {} | TArr {t1: Ty, t2: Ty}
              |   type Val = Unit {} | Var {x: N} | Lam {x: N, e: Exp}
              |   type Exp = EVal {v: Val} | EApp {e1: Exp, e2: Exp}
              |   type OptionVal = SomeVal {v: Val} | NoneVal {}
              |   def eval : Exp -> OptionVal =
              |     case EVal(v:Val) = SomeVal({v = v})
              |     case EApp(e1:Exp, e2:Exp) = if some(eval e1) then
              |       ((lam (v: Val). apply(e2) v) (eval e1).v) else NoneVal({})
              |   def apply(e2: Exp) : Val -> OptionVal =
              |     case Lam(x: N, e: Exp) = eval (subst x e2 e)
              |     case _ = NoneVal({})
              | }
              |
              | Family STLCIf extends STLCBase {
              |   type Ty += TBool {}
              |   type Val += True {} | False {}
              |   type Exp += EIf {e: Exp, e1: Exp, e2: Exp}
              |   def eval: Exp -> OptionVal +=
              |     case EIf(e:Exp, e1:Exp, e2:Exp) =
              |       if some(eval e) 
              |       then ((lam (v: Val). branch(e1, e2) v) (eval e).v)
              |       else NoneVal({})
              |   def branch(e1: Exp, e2: Exp): Val -> OptionVal = 
              |     case True() = eval e1
              |     case False() = eval e2
              |     case _ = NoneVal({})
              | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(List(Prog), prog)
        var stlcbase = Sp(SelfFamily(prog, "STLCBase"))
        var stlcif = Sp(SelfFamily(prog, "STLCIf"))
        assertResult(wfDef(List(prog.sp, stlcbase.sp, stlcif.sp), lkg)){ true }
    }

    





    /*========================== TEST HELPERS ==========================*/


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
        assert(canParse(TestParser.pProgram, fam))
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
        assert(canParse(TestParser.pProgram, fam))
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

    // test("wf - ancestors nonempty") {
    //     var fam = 
    //         """
    //         | Family A {
    //         |   Family K {
    //         |       Family C extends M {
    //         |       }
    //         |   }
    //         |   Family M extends A {
    //         |   }
    //         |}
    //         """.stripMargin
    //     assert(canParse(TestParser.pProgram, fam))
    //     PersimmonLinkages.p = fam
    //     var lkg = computeTypLinkage(List(Prog), Sp(Prog))
    //     var p1 = SelfFamily(Sp(Prog), "A")
    //     var p2 = SelfFamily(Sp(p1), "K")
    //     var p3 = SelfFamily(Sp(p2), "C")
    //     var p4 = SelfFamily(Sp(p1), "M")
    //     assertResult{ancestors(p3).toSet}{
    //         List(p4, p1).toSet
    //     }
    // }



    


}