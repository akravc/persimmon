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
        assertResult(
        computeDefLinkage(List(p1, p2), Sp(p2))
        ){
        DefinitionLinkage(p2, Some(AbsoluteFamily(Sp(Prog), "A")), 
            Map(), Map(), Map(), Map(), Map(), Map())
        }
    }

    /* ============= TYPECHECKER TESTING =============== */

    // TESTING isValue

    test("isvalue: functions") {
        assert(isValue(Lam(Var("x"), BType, Var("x"))))
    }

    test("isvalue: bools") {
        assert(isValue(BExp(true)))
        assert(isValue(BExp(false)))
    }

    test("isvalue: nats") {
        assert(isValue(NExp(0)))
        assert(isValue(NExp(4)))
    }

    test("isvalue: record") {
        assert(isValue(Rec(Map("f"->NExp(2), "p"->BExp(true)))))
    }

    test("not a value: record") {
        assert(!isValue(Rec(Map("f"->Var("x"), "p"->BExp(true)))))
    }

    // A.T({f=2, p=5})
    test("isvalue: instance of a type") {
        assert(isValue(Inst(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), Rec(Map("f"->NExp(2), "p"->NExp(5))))))
    }

    // A.T({f=2, p=x})
    test("not a value: instance of a type") {
        assert(!isValue(Inst(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), Rec(Map("f"->NExp(2), "p"->Var("x"))))))
    }

    // A.T(C {f=2, p=5})
    test("isvalue: instance of an ADT") {
        assert(isValue(InstADT(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), "C", Rec(Map("f"->NExp(2), "p"->NExp(5))))))
    }

    // A.T(C {f=2, p=x})
    test("not a value: instance of an ADT") {
        assert(!isValue(InstADT(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), "C", Rec(Map("f"->NExp(2), "p"->Var("x"))))))
    }

    test("not a value: other") {
        assert(!isValue(App(Var("x"), BExp(true))))
    }

    // TESTING WELL-FORMEDNESS OF TYPES

    test("wfType: nat") {
        assertResult(true)(wfType(List(), NType))
    }

    test("wfType: bool") {
        //assert(wfType(BType, Map()))
        assertResult(true)(wfType(List(), BType))
    }

    // N -> B
    test("wfType: function type") {
        assertResult(true)(wfType(List(), FunType(NType, BType)))
    }

    // {f: B, p: N}
    test("wfType: record type") {
        assertResult(true)(wfType(List(), RecType(Map("f"->BType, "p"->NType))))
    }

    // self(A).T is well formed
    test("wfType: family type") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} 
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(true)(wfType(List(Prog, self_a), PathType(Some(Sp(self_a)), "T")))
    }

    // A.T is well formed
    test("wfType: concrete family type") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} 
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(true)(wfType(List(Prog, self_a), 
        PathType(Some(AbsoluteFamily(Sp(Prog), "A") ), "T")))
    }

    // self(A).K is not well formed
    test("wfType: family type absent") {
        var fam = 
            """
            | Family A {
            |   type T = C1 {} 
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(false)(wfType(List(Prog, self_a), 
        PathType(Some(AbsoluteFamily(Sp(Prog), "A") ), "K")))
    }

    // TESTING SUBTYPING

    test("subtype: the type itself") {
        assertResult(true)(isSubtype(List(), BType, BType))
    }

    test("subtype: the type itself 2") {
        assertResult(true)(isSubtype(List(), NType, NType))
    }

    test("subtype: the type itself 3") {
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(true)(isSubtype(List(), PathType(Some(Sp(self_a)), "G"), PathType(Some(Sp(self_a)), "G")))
    }

    // {f: B, p: N} <: {f: B}
    test("subtype: rectype width") {
        assertResult(true)(isSubtype(List(), RecType(Map("f"->BType, "p"->NType)), RecType(Map("f"->BType))))
    }

    // {g: {f: B, p: N}} <: {g: {f: B}}
    test("subtype: rectype depth") {
        assertResult(true)(isSubtype(List(), RecType(Map("g"->RecType(Map("f"->BType, "p"->NType)))),
        RecType(Map("g"->RecType(Map("f"->BType))))))
    }

    // {f: B, p: N} <: {f: B, g: N}
    test("subtype: rectype bad") {
        assertResult(false)(isSubtype(List(), RecType(Map("f"->BType, "p"->NType)), RecType(Map("f"->BType, "g"->NType))))
    }

    test("subtype: funtype eq") {
        assertResult(true)(isSubtype(List(), FunType(BType,NType), FunType(BType,NType)))
    }

    // {f: B} <: {}, therefore:
    // {} -> {f: B} <: {f: B} -> {}
    test("subtype: funtype good") {
        assertResult(true)(isSubtype(List(), FunType(RecType(Map()), RecType(Map("f"->BType))),
        FunType(RecType(Map("f"->BType)),RecType(Map()))))
    }

    test("subtype: funtype bad") {
        assertResult(false)(isSubtype(List(), FunType(RecType(Map()), RecType(Map("f"->BType))),
        FunType(RecType(Map("f"->BType)),RecType(Map("g"->BType)))))
    }

    test("subtype: PathType good") {
        val self_a = SelfFamily(Sp(Prog), "A")
        var fam = 
            """
            | Family A {
            |   type T = {f: B}
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(true)(isSubtype(List(Prog, self_a), PathType(Some(Sp(self_a)), "T"), RecType(Map("f"->BType))))
    }

    test("subtype: PathType mismatch in linkage") {
        val self_a = SelfFamily(Sp(Prog), "A")
        var fam = 
            """
            | Family A {
            |   type T = {f: B}
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(false)(isSubtype(List(Prog, self_a), PathType(Some(Sp(self_a)), "T"), RecType(Map("g"->BType))))
    }

    test("subtype: PathType bad") {
        val self_a = SelfFamily(Sp(Prog), "A")
        var fam = 
            """
            | Family A {
            |   type T = {f: B}
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(false)(isSubtype(List(Prog, self_a), PathType(Some(Sp(self_a)), "T"), FunType(BType, NType)))
    }

    test("subtype: two unrelated types") {
        assertResult(false)(isSubtype(List(), BType, FunType(BType,NType)))
    }

    // TESTING TYP_INF

    val emptyK = List()
    val emptyG: Map[String, Type] = Map()

    test("getType: nat") {
        assertResult(Some(NType)){getType(emptyK, emptyG, NExp(5))}
    }

    test("getType: bool") {
        assertResult(Some(BType)){getType(emptyK, emptyG, BExp(true))}
        assertResult(Some(BType)){getType(emptyK, emptyG, BExp(false))}
    }

    test("getType: var") {
        assertResult(Some(NType)){getType(emptyK, Map("x"->NType), (Var("x")))}
    }

    test("getType: var none") {
        assertResult(None){getType(emptyK, emptyG, Var("x"))}
    }

    test("getType: lam") {
        assertResult(Some(FunType(BType, NType))){
        getType(emptyK, emptyG, Lam(Var("x"), BType, NExp(5)))
        }
    }

    test("getType: lam identity") {
        assertResult(Some(FunType(BType, BType))){
        getType(emptyK, emptyG, Lam(Var("x"), BType, Var("x")))
        }
    }

    test("getType: app") {
        assertResult(Some(NType)){
        getType(emptyK, emptyG, App(Lam(Var("x"), BType, NExp(5)), BExp(true)))
        }
    }

    test("getType: app improper") {
        assertResult(None){ getType(emptyK, emptyG, App(Var("x"), BExp(true)))}
    }

    test("getType: rec") {
        assertResult(Some(RecType(Map("f"->BType, "p"->NType)))){
        getType(emptyK, emptyG, Rec(Map("f"->BExp(true), "p"->NExp(4))))
        }
    }

    test("getType: rec improper") {
        assertResult(None){ 
            getType(emptyK, emptyG, Rec(Map("f"->BExp(true), "p"->App(NExp(4), BExp(true)))))
        }
    }

    test("getType: rec empty") {
        assertResult(Some(RecType(Map()))){getType(emptyK, emptyG, Rec(Map()))}
    }

    test("getType: proj") {
        assertResult(Some(NType)){
        getType(emptyK, emptyG, Proj(Rec(Map("f"->BExp(true), "p"->NExp(4))), "p"))
        }
    }

    test("getType: proj field absent") {
        assertResult(None){
        getType(emptyK, emptyG, Proj(Rec(Map("f"->BExp(true), "p"->NExp(4))), "g"))}
    }

    test("getType: proj from not record") {
        assertResult(None){
        getType(emptyK, emptyG, Proj(Var("x"), "x"))}
    }

    // self(A).m : (B -> N) = lam (x: B). 5
    test("getType: fam fun") {
        val self_a = SelfFamily(Sp(Prog), "A")
        var fam = 
            """
            | Family A {
            |   val m: (B -> N) = lam (x: B). 5
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(Some(FunType(BType, NType))){
        getType(List(Prog, self_a), emptyG, FamFun(Some(Sp(self_a)), "m"))
        }
    }

    // self(A).m does not exist, we have self(A).g instead
    test("getType: fam fun not in linkage") {
        val self_a = SelfFamily(Sp(Prog), "A")
        var fam = 
            """
            | Family A {
            |   val g: (B -> N) = lam (x: B). 5
            | }
            """.stripMargin
        assert(canParse(TestDefParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(None){
            getType(List(Prog, self_a), emptyG, FamFun(Some(Sp(self_a)), "m"))
        }
    }


//   // self(A).R({f->true, n->5})
//   test("getType: instance of type") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assertResult(Some(PathType(Some(Sp(self_a)), "R"))){
//       getType(emptyK, emptyG, Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     }
//   }

//   test("getType: instance of type wrong field name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "p"->NType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("getType: instance of type wrong field type") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->BType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("getType: instance of type wrong type name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, Inst(PathType(Some(Sp(self_a)), "K"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   // self(A).R(C {f->true, n->5})
//   test("getType: instance of ADT") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assertResult(Some(PathType(Some(Sp(self_a)), "R"))){
//       getType(emptyK, emptyG, InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     }
//   }

//   test("getType: instance of ADT wrong field name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "p"->NExp(5)))))
//     ))
//   }

//   test("getType: instance of ADT wrong field type") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->BType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("getType: instance of ADT wrong constructor name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, InstADT(PathType(Some(Sp(self_a)), "R"), "K", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("getType: instance of ADT wrong type name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, InstADT(PathType(Some(Sp(self_a)), "K"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("getType: instance of ADT empty map in linkage") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map(),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       getType(emptyK, emptyG, InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("getType: match not on instance of ADT") {
//     assert(isLeft(
//       getType(emptyK, emptyG, Match(Var("x"), Var("x")))
//     ))
//   }

//   test("getType: match on instance of type, not ADT") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     // self(A).R({f->true, n->5})
//     val exp = Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5))))
//     assert(isLeft(
//       getType(emptyK, emptyG, Match(exp, exp))
//     ))
//   }

//   test("getType: match on instance of ADT not in linkage") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     // self(A).R({f->true, n->5})
//     val exp = InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5))))
//     assert(isLeft(
//       getType(emptyK, emptyG, Match(exp, exp))
//     ))
//   }


}