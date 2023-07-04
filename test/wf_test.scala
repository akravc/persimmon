import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestParser._
import PrettyPrint._
import PersimmonWF._
import PersimmonUtil.*
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class WFTesting extends AnyFunSuite {
    val prog = Sp(Prog)

    /*===================== MAIN EXPRESSION TESTS =====================*/

    test("wf - ex: eval_ood") {
        val p = readFile("res/eval_ood")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfProg(lkg, PersimmonProgram.exp))
    }
    test("wf - ex: eval_cp") {
        val p = readFile("res/eval_cp")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfProg(lkg, PersimmonProgram.exp))
    }

    test("wf - ex: abcode_main") {
        val p = readFile("res/abcode_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfProg(lkg, PersimmonProgram.exp))
    }

    test("wf - ex: default_main") {
        val p = readFile("res/default_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfProg(lkg, PersimmonProgram.exp))
    }

    test("wf - ex: even_odd_main") {
        val p = readFile("res/even_odd_main")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfProg(lkg, PersimmonProgram.exp))
    }


    /*===================== LARGE & PAPER EXAMPLES =====================*/

    test("wf - ex: peano") {
        val p = readFile("res/peano")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: ab") {
        val p = readFile("res/ab")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcode") {
        val p = readFile("res/abcode")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcode_dot") {
        val p = readFile("res/abcode_dot")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodepaper_dot") {
        val p = readFile("res/abcodepaper_dot")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcode_vars") {
        val p = readFile("res/abcode_vars")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodeover") {
        val p = readFile("res/abcodeover")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodeovern") {
        val p = readFile("res/abcodeovern")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodepaper") {
        val p = readFile("res/abcodepaper")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodepaper2") {
        val p = readFile("res/abcodepaper2")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodepaper2sugar") {
        val p = readFile("res/abcodepaper2sugar")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodepaper2sugar2") {
        val p = readFile("res/abcodepaper2sugar2")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: abcodepaper2sugar2b") {
        val p = readFile("res/abcodepaper2sugar2b")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: default") {
        val p = readFile("res/default")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: even_odd") {
        val p = readFile("res/even_odd")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: comment") {
        val p = readFile("res/comment")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    // test("wf - ex: example") {
    //   val p = readFile("res/example")
    //   PersimmonLinkages.p = p
        // val lkg = computeDefLinkage(prog)
        // assertResult(true)(wfDef(List(Prog), lkg))
    // }
    test("wf - ex: matcherr") {
        val p = readFile("res/matcherr")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(false)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins") {
        val p = readFile("res/mixins")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins0") {
        val p = readFile("res/mixins0")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins00") {
        val p = readFile("res/mixins00")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins1") {
        val p = readFile("res/mixins1")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins2") {
        val p = readFile("res/mixins2")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins_sug") {
        val p = readFile("res/mixins_sug")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins0_sug") {
        val p = readFile("res/mixins0_sug")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins00_sug") {
        val p = readFile("res/mixins00_sug")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: mixins1_sug") {
        val p = readFile("res/mixins1_sug")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    // test("wf - ex: pretty_example") {
    //   val p = readFile("res/pretty_example")
    //   PersimmonLinkages.p = p
    //   val lkg = computeDefLinkage(prog)
    //   assertResult(true)(wfDef(List(Prog), lkg))
    // }
    test("wf - ex: reso") {
        val p = readFile("res/reso")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: stlc0") {
        val p = readFile("res/stlc0")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: stlc1") {
        val p = readFile("res/stlc1")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: stlc") {
        val p = readFile("res/stlc")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: stlc_dot") {
        val p = readFile("res/stlc_dot")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test1") {
        val p = readFile("res/test1")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test1b") {
        val p = readFile("res/test1b")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test2") {
        val p = readFile("res/test2")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test3") {
        val p = readFile("res/test3")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test4") {
        val p = readFile("res/test4")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test5") {
        val p = readFile("res/test5")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: test6") {
        val p = readFile("res/test6")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: triple") {
        val p = readFile("res/triple")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: wrapper") {
        val p = readFile("res/wrapper")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: wrapper2") {
        val p = readFile("res/wrapper2")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: wrapper3") {
        val p = readFile("res/wrapper3")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }
    test("wf - ex: wrapper4") {
        val p = readFile("res/wrapper4")
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(true)(wfDef(List(Prog), lkg))
    }

    /*========================== SMALL EXAMPLES ==========================*/

    test("wf - ex: regression1") {
        val p = """
        Family A {
        type T = O {}
        val f: T -> N = lam(t: T).t.n // bogus
        }
        """
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(false)(wfDef(List(Prog), lkg))
    }

    test("wf - ex: regression2") {
        val p = """
        Family A {
        type T = O {}
        val f: T -> {n: N} = lam(t: T).t // bogus
        }
        """
        PersimmonLinkages.p = p
        val lkg = computeDefLinkage(prog)
        assertResult(false)(wfDef(List(Prog), lkg))
    }

    test("wf - basic prog lkg") {
        val fam = 
            """
            | Family A {
            |
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - type") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - type - duplicate constructors") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} | C1 {}
            |}
            """.stripMargin
        // duplicate constructor names, error thrown while parsing
        assertThrows[Exception](canParse(TestParser.pProgram, fam))
    }

    test("wf - type - duplicate fields") {
        val fam = 
            """
            | Family A {
            |   type T = {a: N, a: B}
            |}
            """.stripMargin
        // duplicate field names, error thrown while parsing
        assertThrows[Exception](canParse(TestParser.pProgram, fam))
    }

    test("wf - type - duplicate type names") {
        val fam = 
            """
            | Family A {
            |   type T = {a: N, b: B}
            |   type T = C1 {}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(false)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - function") {
        val fam = 
            """
            | Family A {
            |   val g: N -> B = lam (x: N). true
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(Sp(Prog))
        val prog = Sp(Prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - function call") {
        val fam = 
            """
            | Family A {
            |   val goo: N -> B = lam (x: N). true
            |   val foo: N -> B = lam (y: N). (goo y)
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(Sp(Prog))
        val prog = Sp(Prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - cases") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   cases c <T> : {} -> {C1: {} -> N, C2: {} -> N} = 
            |       lam (x: {}). {C1 = lam (_: {}). 1, C2 = lam (_:{}). 2 }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - cases ext") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   cases c <T> : {} -> {C1: {} -> N, C2: {} -> N} = 
            |       lam (x: {}). {C1 = lam (_: {}). 1, C2 = lam (_:{}). 2 }
            |}
            |
            |Family A2 extends A {
            |   type T += C3 {} 
            |   cases c <T> : {} -> {C3: {} -> N} += 
            |       lam (x: {}). {C3 = lam (_: {}). 3}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val a = Sp(SelfFamily(prog, "A"))
        val a2 = Sp(SelfFamily(prog, "A2"))
        val lkg = computeDefLinkage(a2)
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - cases missing def") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   val f: T -> N = lam (x: T). match x with <c> {}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(false)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - cases + match in fun") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   val f: T -> N = lam (x: T). match x with <c> {}
            |   cases c <T> : {} -> {C1: {} -> N, C2: {} -> N} =
            |       lam (x: {}). 
            |           {C1 = lam (_: {}). 1, C2 = lam (_:{}). 2 }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - cases sugar") {
        val fam = 
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
        val lkg = computeDefLinkage(prog)
        val a = Sp(SelfFamily(prog, "A"))
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }

    test("wf - extend type") {
        val fam = 
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
        val a = Sp(SelfFamily(prog, "A"))
        val c = Sp(SelfFamily(prog, "C"))
        val lkg = computeDefLinkage(c)
        assertResult(true)(wfDef(List(prog.sp, a.sp), lkg))
    }



    /*========================== TEST HELPERS ==========================*/


    test("wf - collect all paths") {
        val fam = 
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
        val lkg = computeTypLinkage(Sp(Prog))
        val p1 = SelfFamily(Sp(Prog), "A")
        val p2 = SelfFamily(Sp(p1), "K")
        val p3 = SelfFamily(Sp(p2), "C")
        val p4 = SelfFamily(Sp(p1), "M")
        assertResult{collectAllPathsWithin(lkg).toSet}{
            List(p1, p2, p3, p4, Prog).toSet
        }
    }

    test("wf - ancestors empty") {
        val fam = 
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
        val lkg = computeTypLinkage(Sp(Prog))
        val p1 = SelfFamily(Sp(Prog), "A")
        val p2 = SelfFamily(Sp(p1), "K")
        val p3 = SelfFamily(Sp(p2), "C")
        val p4 = SelfFamily(Sp(p1), "M")
        assertResult{ancestors(p3).toSet}{
            List().toSet
        }
    }

    test("wf - ancestors nonempty") {
        val fam = 
            """
            | Family A {
            |   Family K {
            |       Family C extends A.M {
            |       }
            |   }
            |   Family M extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val lkg = computeTypLinkage(Sp(Prog))
        val p1 = SelfFamily(Sp(Prog), "A")
        val p2 = SelfFamily(Sp(p1), "K")
        val p3 = SelfFamily(Sp(p2), "C")
        val p4 = SelfFamily(Sp(p1), "M")
        assertResult{ancestors(p3).toSet}{
            List(p4, p1).toSet
        }
    }

}
