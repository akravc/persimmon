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
        // assertResult(
        // computeDefLinkage(List(p1, p2), Sp(p2))
        // ){
        // DefinitionLinkage(Sp(p2), p2, Some(p1), 
        //     Map(), Map(), Map(), Map(), Map(), Map())
        // }
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

//   // TESTING TYP_INF

//   val emptyLinkage = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(), Map())

//   val typInf = typeOfExpression(emptyLinkage, Map())

//   test("typinf: nat") {
//     assertResult(Right(NType)){typInf(NExp(5))}
//   }

//   test("typinf: bool") {
//     assertResult(Right(BType)){typInf(BExp(true))}
//     assertResult(Right(BType)){typInf(BExp(false))}
//   }

//   test("typinf: var") {
//     assertResult(Right(NType)){typeOfExpression(emptyLinkage, Map("x"->NType))(Var("x"))}
//   }

//   test("typinf: var none") {
//     assertResult(Left("Variable x unbound\nIn expression x")){typInf(Var("x"))}
//   }

//   test("typinf: lam") {
//     assertResult(Right(FunType(BType, NType))){
//       typInf(Lam(Var("x"), BType, NExp(5)))
//     }
//   }

//   test("typinf: lam identity") {
//     assertResult(Right(FunType(BType, BType))){
//       typInf(Lam(Var("x"), BType, Var("x")))
//     }
//   }

//   test("typinf: app") {
//     assertResult(Right(NType)){
//       typInf(App(Lam(Var("x"), BType, NExp(5)), BExp(true)))
//     }
//   }

//   test("typinf: app improper") {
//     assertResult(Left("Variable x unbound\nIn expression x\nIn expression (x true)")){
//       typInf(App(Var("x"), BExp(true)))
//     }
//   }

//   test("typinf: rec") {
//     assertResult(Right(RecType(Map("f"->BType, "p"->NType)))){
//       typInf(Rec(Map("f"->BExp(true), "p"->NExp(4))))
//     }
//   }

//   def isLeft[A,B](x: Either[A,B]) = x match {
//     case Left(_) => true
//     case Right(_) => false
//   }

//   test("typinf: rec improper") {
//     assert(isLeft(
//       typInf(Rec(Map("f"->BExp(true), "p"->App(NExp(4), BExp(true)))))))
//   }

//   test("typinf: rec empty") {
//     assertResult(Right(RecType(Map()))){typInf(Rec(Map()))}
//   }

//   test("typinf: proj") {
//     assertResult(Right(NType)){
//       typInf(Proj(Rec(Map("f"->BExp(true), "p"->NExp(4))), "p"))
//     }
//   }

//   test("typinf: proj field absent") {
//     assert(isLeft(
//       typInf(Proj(Rec(Map("f"->BExp(true), "p"->NExp(4))), "g"))))
//   }

//   test("typinf: proj from not record") {
//     assert(isLeft(
//       typInf(Proj(Var("x"), "x"))))
//   }

//   // self(A).m : (B -> N) = lam (x: B). 5
//   test("typinf: fam fun") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(), Map(),
//         Map("m"->(FunDefn("m", FunType(BType, NType), DefnBody(Some(Lam(Var("x"), BType, NExp(5))), None, None)))),
//         Map(), Map())))
//     init(k)
//     assertResult(Right(FunType(BType, NType))){
//       typInf(FamFun(Some(Sp(self_a)), "m"))
//     }
//   }

//   // self(A).m does not exist, we have self(A).g instead
//   test("typinf: fam fun not in linkage") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(), Map(),
//         Map("g"->(FunDefn("g", FunType(BType, NType), DefnBody(Some(Lam(Var("x"), BType, NExp(5))), None, None)))),
//         Map(), Map())))
//     init(k)
//     assertResult(Left("No such function m\nIn expression self(<>.A).m")){
//       typInf(FamFun(Some(Sp(self_a)), "m"))
//     }
//   }

//   test("typinf: fam fun, absent linkage for self_a") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     assert(isLeft(
//       typInf(FamFun(Some(Sp(self_a)), "m"))
//     ))
//   }

//   // self(A).R({f->true, n->5})
//   test("typinf: instance of type") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assertResult(Right(PathType(Some(Sp(self_a)), "R"))){
//       typInf(Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     }
//   }

//   test("typinf: instance of type wrong field name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "p"->NType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("typinf: instance of type wrong field type") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->BType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("typinf: instance of type wrong type name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map("R"->(TypeDefn("R", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None)))), Map(), Map(), Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(Inst(PathType(Some(Sp(self_a)), "K"), Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   // self(A).R(C {f->true, n->5})
//   test("typinf: instance of ADT") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assertResult(Right(PathType(Some(Sp(self_a)), "R"))){
//       typInf(InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     }
//   }

//   test("typinf: instance of ADT wrong field name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "p"->NExp(5)))))
//     ))
//   }

//   test("typinf: instance of ADT wrong field type") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->BType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("typinf: instance of ADT wrong constructor name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(InstADT(PathType(Some(Sp(self_a)), "R"), "K", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("typinf: instance of ADT wrong type name") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map("R"->(AdtDefn("R", Eq, DefnBody(Some(Map("C"->RecType(Map("f"->BType, "n"->NType)))), None, None)))),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(InstADT(PathType(Some(Sp(self_a)), "K"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("typinf: instance of ADT empty map in linkage") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     val k = Linkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
//       Map("A" -> Linkage(AbsoluteFamily(Sp(Prog), "A"), self_a, None,
//         Map(), Map(),
//         Map(),
//         Map(), Map(), Map())))
//     init(k)
//     assert(isLeft(
//       typInf(InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5)))))
//     ))
//   }

//   test("typinf: match not on instance of ADT") {
//     assert(isLeft(
//       typInf(Match(Var("x"), Var("x")))
//     ))
//   }

//   test("typinf: match on instance of type, not ADT") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     // self(A).R({f->true, n->5})
//     val exp = Inst(PathType(Some(Sp(self_a)), "R"), Rec(Map("f"->BExp(true), "n"->NExp(5))))
//     assert(isLeft(
//       typInf(Match(exp, exp))
//     ))
//   }

//   test("typinf: match on instance of ADT not in linkage") {
//     val self_a = SelfFamily(Sp(Prog), "A")
//     // self(A).R({f->true, n->5})
//     val exp = InstADT(PathType(Some(Sp(self_a)), "R"), "C", Rec(Map("f"->BExp(true), "n"->NExp(5))))
//     assert(isLeft(
//       typInf(Match(exp, exp))
//     ))
//   }


}