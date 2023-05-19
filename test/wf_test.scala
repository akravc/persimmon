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
    var prog = Sp(Prog)

    /*===================== LARGE & PAPER EXAMPLES =====================*/

    test("wf - ex: peano") {
        val p = readFile("res/peano")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: ab") {
        val p = readFile("res/ab")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcode") {
        val p = readFile("res/abcode")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodeover") {
        val p = readFile("res/abcodeover")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodeovern") {
        val p = readFile("res/abcodeovern")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodepaper") {
        val p = readFile("res/abcodepaper")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodepaper2") {
        val p = readFile("res/abcodepaper2")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodepaper2sugar") {
        val p = readFile("res/abcodepaper2sugar")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodepaper2sugar2") {
        val p = readFile("res/abcodepaper2sugar2")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: abcodepaper2sugar2b") {
        val p = readFile("res/abcodepaper2sugar2b")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: default") {
        val p = readFile("res/default")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: even_odd") {
        val p = readFile("res/even_odd")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: comment") {
        val p = readFile("res/comment")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    // test("wf - ex: example") {
    //   val p = readFile("res/example")
    //   PersimmonLinkages.p = p
        // var lkg = computeDefLinkage(prog)
        // assertResult(wfDef(List(Prog), lkg)){ true }
    // }
    test("wf - ex: mixins") {
        val p = readFile("res/mixins")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: mixins0") {
        val p = readFile("res/mixins0")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: mixins00") {
        val p = readFile("res/mixins00")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: mixins1") {
        val p = readFile("res/mixins1")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: mixins2") {
        val p = readFile("res/mixins2")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    // test("wf - ex: pretty_example") {
    //   val p = readFile("res/pretty_example")
    //   PersimmonLinkages.p = p
        // var lkg = computeDefLinkage(prog)
        // assertResult(wfDef(List(Prog), lkg)){ true }
    // }
    test("wf - ex: reso") {
        val p = readFile("res/reso")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: stlc") {
        val p = readFile("res/stlc")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test1") {
        val p = readFile("res/test1")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test1b") {
        val p = readFile("res/test1b")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test2") {
        val p = readFile("res/test2")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test3") {
        val p = readFile("res/test3")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test4") {
        val p = readFile("res/test4")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test5") {
        val p = readFile("res/test5")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: test6") {
        val p = readFile("res/test6")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: triple") {
        val p = readFile("res/triple")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: wrapper") {
        val p = readFile("res/wrapper")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: wrapper2") {
        val p = readFile("res/wrapper2")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: wrapper3") {
        val p = readFile("res/wrapper3")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }
    test("wf - ex: wrapper4") {
        val p = readFile("res/wrapper4")
        PersimmonLinkages.p = p
        var lkg = computeDefLinkage(prog)
        assertResult(wfDef(List(Prog), lkg)){ true }
    }

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
        var lkg = computeDefLinkage(prog)
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
        var lkg = computeDefLinkage(prog)
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
        var lkg = computeDefLinkage(prog)
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
        var lkg = computeDefLinkage(Sp(Prog))
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
        var lkg = computeDefLinkage(Sp(Prog))
        var prog = Sp(Prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - cases") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   cases c <T> : {} -> {C1: {} -> N, C2: {} -> N} = 
            |       lam (x: {}). {C1 = lam (_: {}). 1, C2 = lam (_:{}). 2 }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - cases ext") {
        var fam = 
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
        var a = Sp(SelfFamily(prog, "A"))
        var a2 = Sp(SelfFamily(prog, "A2"))
        var lkg = computeDefLinkage(a2)
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
    }

    test("wf - cases missing def") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} | C2 {}
            |   val f: T -> N = lam (x: T). match x with <c> {}
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeDefLinkage(prog)
        var a = Sp(SelfFamily(prog, "A"))
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ false }
    }

    test("wf - cases + match in fun") {
        var fam = 
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
        var lkg = computeDefLinkage(prog)
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
        var lkg = computeDefLinkage(prog)
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
        var a = Sp(SelfFamily(prog, "A"))
        var c = Sp(SelfFamily(prog, "C"))
        var lkg = computeDefLinkage(c)
        assertResult(wfDef(List(prog.sp, a.sp), lkg)){ true }
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
        var lkg = computeTypLinkage(Sp(Prog))
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
        var lkg = computeTypLinkage(Sp(Prog))
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
            |       Family C extends A.M {
            |       }
            |   }
            |   Family M extends A {
            |   }
            |}
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        var lkg = computeTypLinkage(Sp(Prog))
        var p1 = SelfFamily(Sp(Prog), "A")
        var p2 = SelfFamily(Sp(p1), "K")
        var p3 = SelfFamily(Sp(p2), "C")
        var p4 = SelfFamily(Sp(p1), "M")
        assertResult{ancestors(p3).toSet}{
            List(p4, p1).toSet
        }
    }

}
