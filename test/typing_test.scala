import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._ 
import PersimmonTyping._ 
import TestParser._
import PrettyPrint._
import PersimmonWF._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class TypecheckerTesting extends AnyFunSuite {

    /* ============= TYPECHECKER TESTING =============== */

    // TESTING isValue

    test("typing - isvalue: functions") {
        assert(isValue(Lam(Var("x"), BType, Var("x"))))
    }

    test("typing - isvalue: bools") {
        assert(isValue(BExp(true)))
        assert(isValue(BExp(false)))
    }

    test("typing - isvalue: nats") {
        assert(isValue(NExp(0)))
        assert(isValue(NExp(4)))
    }

    test("typing - isvalue: record") {
        assert(isValue(Record(Map("f"->NExp(2), "p"->BExp(true)))))
    }

    test("typing - not a value: record") {
        assert(!isValue(Record(Map("f"->Var("x"), "p"->BExp(true)))))
    }

    // A.T({f=2, p=5})
    test("typing - isvalue: instance of a type") {
        assert(isValue(Inst(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), Record(Map("f"->NExp(2), "p"->NExp(5))))))
    }

    // A.T({f=2, p=x})
    test("typing - not a value: instance of a type") {
        assert(!isValue(Inst(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), Record(Map("f"->NExp(2), "p"->Var("x"))))))
    }

    // A.T(C {f=2, p=5})
    test("typing - isvalue: instance of an ADT") {
        assert(isValue(InstADT(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), "C", Record(Map("f"->NExp(2), "p"->NExp(5))))))
    }

    // A.T(C {f=2, p=x})
    test("typing - not a value: instance of an ADT") {
        assert(!isValue(InstADT(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T"), "C", Record(Map("f"->NExp(2), "p"->Var("x"))))))
    }

    test("typing - not a value: other") {
        assert(!isValue(App(Var("x"), BExp(true))))
    }

    // TESTING WELL-FORMEDNESS OF TYPES

    test("typing - wfType: nat") {
        assertResult(true)(wfType(List(), NType))
    }

    test("typing - wfType: bool") {
        //assert(wfType(BType, Map()))
        assertResult(true)(wfType(List(), BType))
    }

    // N -> B
    test("typing - wfType: function type") {
        assertResult(true)(wfType(List(), FunType(NType, BType)))
    }

    // {f: B, p: N}
    test("typing - wfType: record type") {
        assertResult(true)(wfType(List(), RecordType(Map("f"->BType, "p"->NType))))
    }

    // self(A).T is well formed
    test("typing - wfType: family type") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} 
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(true)(wfType(List(Prog, self_a), PathType(Some(Sp(self_a)), "T")))
    }

    // A.T is well formed
    test("typing - wfType: concrete family type") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} 
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(true)(wfType(List(Prog, self_a), 
        PathType(Some(AbsoluteFamily(Sp(Prog), "A") ), "T")))
    }

    // self(A).K is not well formed
    test("typing - wfType: family type absent") {
        val fam = 
            """
            | Family A {
            |   type T = C1 {} 
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(false)(wfType(List(Prog, self_a), 
        PathType(Some(AbsoluteFamily(Sp(Prog), "A") ), "K")))
    }

    // TESTING SUBTYPING

    test("typing - subtype: the type itself") {
        assertResult(true)(isSubtype(List(), BType, BType))
    }

    test("typing - subtype: the type itself 2") {
        assertResult(true)(isSubtype(List(), NType, NType))
    }

    test("typing - subtype: the type itself 3") {
        val self_a = SelfFamily(Sp(Prog), "A") // path self(A)
        assertResult(true)(isSubtype(List(), PathType(Some(Sp(self_a)), "G"), PathType(Some(Sp(self_a)), "G")))
    }

    // {f: B, p: N} <: {f: B}
    test("typing - subtype: rectype width") {
        assertResult(true)(isSubtype(List(), RecordType(Map("f"->BType, "p"->NType)), RecordType(Map("f"->BType))))
    }

    // {g: {f: B, p: N}} <: {g: {f: B}}
    test("typing - subtype: rectype depth") {
        assertResult(true)(isSubtype(List(), RecordType(Map("g"->RecordType(Map("f"->BType, "p"->NType)))),
        RecordType(Map("g"->RecordType(Map("f"->BType))))))
    }

    // {f: B, p: N} <: {f: B, g: N}
    test("typing - subtype: rectype bad") {
        assertResult(false)(isSubtype(List(), RecordType(Map("f"->BType, "p"->NType)), RecordType(Map("f"->BType, "g"->NType))))
    }

    test("typing - subtype: funtype eq") {
        assertResult(true)(isSubtype(List(), FunType(BType,NType), FunType(BType,NType)))
    }

    // {f: B} <: {}, therefore:
    // {} -> {f: B} <: {f: B} -> {}
    test("typing - subtype: funtype good") {
        assertResult(true)(isSubtype(List(), FunType(RecordType(Map()), RecordType(Map("f"->BType))),
        FunType(RecordType(Map("f"->BType)),RecordType(Map()))))
    }

    test("typing - subtype: funtype bad") {
        assertResult(false)(isSubtype(List(), FunType(RecordType(Map()), RecordType(Map("f"->BType))),
        FunType(RecordType(Map("f"->BType)),RecordType(Map("g"->BType)))))
    }

    test("typing - subtype: PathType good") {
        val self_a = SelfFamily(Sp(Prog), "A")
        val fam = 
            """
            | Family A {
            |   type T = {f: B}
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(true)(isSubtype(List(Prog, self_a), PathType(Some(Sp(self_a)), "T"), RecordType(Map("f"->BType))))
    }

    test("typing - subtype: PathType mismatch in linkage") {
        val self_a = SelfFamily(Sp(Prog), "A")
        val fam = 
            """
            | Family A {
            |   type T = {f: B}
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(false)(isSubtype(List(Prog, self_a), PathType(Some(Sp(self_a)), "T"), RecordType(Map("g"->BType))))
    }

    test("typing - subtype: PathType bad") {
        val self_a = SelfFamily(Sp(Prog), "A")
        val fam = 
            """
            | Family A {
            |   type T = {f: B}
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(false)(isSubtype(List(Prog, self_a), PathType(Some(Sp(self_a)), "T"), FunType(BType, NType)))
    }

    test("typing - subtype: two unrelated types") {
        assertResult(false)(isSubtype(List(), BType, FunType(BType,NType)))
    }

    // TESTING TYP_INF

    val emptyK = List()
    val emptyG: Map[String, Type] = Map()

    test("typing - getType: nat") {
        assertResult(Right(NType)){getType(emptyK, emptyG, NExp(5))}
    }

    test("typing - getType: bool") {
        assertResult(Right(BType)){getType(emptyK, emptyG, BExp(true))}
        assertResult(Right(BType)){getType(emptyK, emptyG, BExp(false))}
    }

    test("typing - getType: var") {
        assertResult(Right(NType)){getType(emptyK, Map("x"->NType), (Var("x")))}
    }

    test("typing - getType: var none") {
        assertResult(Left("Variable x is unbound")){getType(emptyK, emptyG, Var("x"))}
    }

    test("typing - getType: lam") {
        assertResult(Right(FunType(BType, NType))){
        getType(emptyK, emptyG, Lam(Var("x"), BType, NExp(5)))
        }
    }

    test("typing - getType: lam identity") {
        assertResult(Right(FunType(BType, BType))){
        getType(emptyK, emptyG, Lam(Var("x"), BType, Var("x")))
        }
    }

    test("typing - getType: app") {
        assertResult(Right(NType)){
        getType(emptyK, emptyG, App(Lam(Var("x"), BType, NExp(5)), BExp(true)))
        }
    }

    test("typing - getType: app improper") {
        assertResult(Left("Variable x is unbound")){ getType(emptyK, emptyG, App(Var("x"), BExp(true)))}
    }

    test("typing - getType: rec") {
        assertResult(Right(RecordType(Map("f"->BType, "p"->NType)))){
        getType(emptyK, emptyG, Record(Map("f"->BExp(true), "p"->NExp(4))))
        }
    }

    test("typing - getType: rec improper") {
        assertResult(Left("Type N is not a function type in expression (4 true)")){ 
            getType(emptyK, emptyG, Record(Map("f"->BExp(true), "p"->App(NExp(4), BExp(true)))))
        }
    }

    test("typing - getType: rec empty") {
        assertResult(Right(RecordType(Map()))){getType(emptyK, emptyG, Record(Map()))}
    }

    test("typing - getType: proj") {
        assertResult(Right(NType)){
        getType(emptyK, emptyG, Proj(Record(Map("f"->BExp(true), "p"->NExp(4))), "p"))
        }
    }

    test("typing - getType: proj field absent") {
        assertResult(Left("Missing field g")){
        getType(emptyK, emptyG, Proj(Record(Map("f"->BExp(true), "p"->NExp(4))), "g"))}
    }

    test("typing - getType: proj from not record") {
        assertResult(Left("Cannot project from type N")){
        getType(emptyK, emptyG, Proj(NExp(1), "x"))}
    }

    // self(A).m : (B -> N) = lam (x: B). 5
    test("typing - getType: fam fun") {
        val self_a = SelfFamily(Sp(Prog), "A")
        val fam = 
            """
            | Family A {
            |   val m: (B -> N) = lam (x: B). 5
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(Right(FunType(BType, NType))){
        getType(List(Prog, self_a), emptyG, FamFun(Some(Sp(self_a)), "m"))
        }
    }

    // self(A).m does not exist, we have self(A).g instead
    test("typing - getType: fam fun not in linkage") {
        val self_a = SelfFamily(Sp(Prog), "A")
        val fam = 
            """
            | Family A {
            |   val g: (B -> N) = lam (x: B). 5
            | }
            """.stripMargin
        assert(canParse(TestParser.pProgram, fam))
        PersimmonLinkages.p = fam
        assertResult(Left("Linkage missing m in expression self(<>.A).m")){
            getType(List(Prog, self_a), emptyG, FamFun(Some(Sp(self_a)), "m"))
        }
    }
}
