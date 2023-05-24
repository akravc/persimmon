import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestParser._
import PrettyPrint._
import PersimmonUtil.*
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File


class ParserTesting extends AnyFunSuite {

  /* ============= TEST RES EXAMPLES ============= */

  test("parse - ex: peano") {
    val p = readFile("res/peano")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: ab") {
    val p = readFile("res/ab")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcode") {
    val p = readFile("res/abcode")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcode_vars") {
    val p = readFile("res/abcode_vars")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodeover") {
    val p = readFile("res/abcodeover")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodeovern") {
    val p = readFile("res/abcodeovern")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodepaper") {
    val p = readFile("res/abcodepaper")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodepaper2") {
    val p = readFile("res/abcodepaper2")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodepaper2sugar") {
    val p = readFile("res/abcodepaper2sugar")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodepaper2sugar2") {
    val p = readFile("res/abcodepaper2sugar2")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: abcodepaper2sugar2b") {
    val p = readFile("res/abcodepaper2sugar2b")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: default") {
    val p = readFile("res/default")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: even_odd") {
    val p = readFile("res/even_odd")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: comment") {
    val p = readFile("res/comment")
    assert(canParse(pProgram, p))
  }
  // test("parse - ex: example") {
  //   val p = readFile("res/example")
  //   assert(canParse(pProgram, p))
  // }
  test("parse - ex: matcherr") {
    val p = readFile("res/matcherr")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins") {
    val p = readFile("res/mixins")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins0") {
    val p = readFile("res/mixins0")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins00") {
    val p = readFile("res/mixins00")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins1") {
    val p = readFile("res/mixins1")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins2") {
    val p = readFile("res/mixins2")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins_sug") {
    val p = readFile("res/mixins_sug")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins0_sug") {
    val p = readFile("res/mixins0_sug")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins00_sug") {
    val p = readFile("res/mixins00_sug")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: mixins1_sug") {
    val p = readFile("res/mixins1_sug")
    assert(canParse(pProgram, p))
  }
  // test("parse - ex: pretty_example") {
  //   val p = readFile("res/pretty_example")
  //   assert(canParse(pProgram, p))
  // }
  test("parse - ex: reso") {
    val p = readFile("res/reso")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: stlc") {
    val p = readFile("res/stlc")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test1") {
    val p = readFile("res/test1")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test1b") {
    val p = readFile("res/test1b")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test2") {
    val p = readFile("res/test2")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test3") {
    val p = readFile("res/test3")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test4") {
    val p = readFile("res/test4")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test5") {
    val p = readFile("res/test5")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: test6") {
    val p = readFile("res/test6")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: triple") {
    val p = readFile("res/triple")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: wrapper") {
    val p = readFile("res/wrapper")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: wrapper2") {
    val p = readFile("res/wrapper2")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: wrapper3") {
    val p = readFile("res/wrapper3")
    assert(canParse(pProgram, p))
  }
  test("parse - ex: wrapper4") {
    val p = readFile("res/wrapper4")
    assert(canParse(pProgram, p))
  }


  /* ============= TEST PARSER ============= */

  // Parsing Paths
  test("parse - paths: absolute path") {
    val inp = "A.C.D"
    assert(canParse(pPath, inp))
    assertResult(
      AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),"D")
    ){parseSuccess(pPath, inp)}
  }

  test("parse - paths: self prefixed absolute path") {
    val inp = "self(self(A).C).D"
    assert(canParse(pPath, inp))
    assertResult(
      AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")
    ){parseSuccess(pPath, inp)}
  }

  test("parse - paths: self path") {
    val inp = "self(self(self(A).C).D)"
    assert(canParse(pPath, inp))
    assertResult(
      Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))
    ){parseSuccess(pPath, inp)}
  }

  // Parsing Types
  test("parse - types: nat") {
    assert(canParse(pType, "N"))
    assertResult(NType){parseSuccess(pType, "N")}
  }

  test("parse - types: bool") {
    assert(canParse(pType, "B"))
    assertResult(BType){parseSuccess(pType, "B")}
  }

  test("parse - types: arrow") {
    assert(canParse(pType, "B -> N"))
    assertResult(FunType(BType, NType)){parseSuccess(pType, "B -> N")}
  }

  test("parse - types: absolute path type") {
    assert(canParse(pType, "A.R"))
    assertResult(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")){parseSuccess(pType, "A.R")}
  }

  test("parse - types: absolute path path type") {
    val inp = "A.C.D.R"
    assert(canParse(pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D")), "R")
    ){parseSuccess(pType, inp)}
  }

  test("parse - types: absolute path self head path type") {
    val inp = "self(self(A).C).D.R"
    assert(canParse(pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")), "R")
    ) {parseSuccess(pType, inp)}
  }

  test("parse - types: self path type") {
    assert(canParse(pType, "self(A).R"))
    assertResult(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "R")){parseSuccess(pType, "self(A).R")}
  }

  test("parse - types: self path path type") {
    val inp = "self(self(self(A).C).D).R"
    assert(canParse(pType, inp))
    assertResult(
      PathType(Some(Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))), "R")
    ){parseSuccess(pType, inp)}
  }

  test("parse - types: record type") {
    assert(canParse(pType, "{ a: N, b: B, c: A.R }"))
    assertResult(
      RecordType(Map("a"->NType, "b"->BType, "c"->PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")))
    ){parseSuccess(pType, "{ a: N, b: B, c: A.R }")}
  }

  test("parse - types: paren form") {
    assert(canParse(pType, "(B->{})"))
    assertResult(FunType(BType, RecordType(Map()))){parseSuccess(pType, "(B->{})")}
  }

  // Parsing Expressions
  test("parse - exp: true") {
    assert(canParse(pExp, "true"))
    assertResult(BExp(true)){parseSuccess(pExp, "true")}
  }

  test("parse - exp: false") {
    assert(canParse(pExp, "false"))
    assertResult(BExp(false)){parseSuccess(pExp, "false")}
  }

  test("parse - exp: nat") {
    assert(canParse(pExp, "5"))
    assertResult(NExp(5)){parseSuccess(pExp, "5")}
  }

  test("parse - exp: var") {
    assert(canParse(pExp, "x"))
    assertResult(Var("x")){parseSuccess(pExp, "x")}
  }

  test("parse - exp: lam") {
    assert(canParse(pExp, "lam (x: B). x"))
    assertResult(Lam(Var("x"), BType, Var("x"))){parseSuccess(pExp, "lam (x: B). x")}
  }

  test("parse - exp: select function from family") {
    assert(canParse(pExp, "self(A).calculate"))
    assertResult(FamFun(Some(Sp(SelfFamily(Sp(Prog), "A"))), "calculate")){parseSuccess(pExp, "self(A).calculate")}
  }

  test("parse - exp: app") {
    assert(canParse(pExp, "(lam (x: B). x) true"))
    assertResult(App(Lam(Var("x"), BType, Var("x")), BExp(true))){parseSuccess(pExp, "(lam (x: B). x) true")}
    
  }

  test("parse - exp: record") {
    assert(canParse(pExp, "{ a = 5 , b = true }"))
    assertResult(Record(Map("a"-> NExp(5), "b" -> BExp(true)))){parseSuccess(pExp, "{ a = 5, b = true }")}
  }

  test("parse - exp: projection") {
    assert(canParse(pExp, "{ a = 5 , b = true }.b"))
    assertResult(Proj(Record(Map("a"-> NExp(5), "b" -> BExp(true))), "b")){parseSuccess(pExp, "{ a = 5 , b = true }.b")}
  }

  test("parse - exp: instance") {
    assert(canParse(pExp, "A.R({a = 4})"))
    assertResult(
      Inst(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), Record(Map("a"->NExp(4))))
    ){parseSuccess(pExp, "A.R({a = 4})")}
  }

  test("parse - exp: ADT instance") {
    assert(canParse(pExp, "A.R(C {})"))
    assertResult(
      InstADT(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), "C", Record(Map()))
    ){parseSuccess(pExp, "A.R(C {})")}
  }

  test("parse - parser: cases with underscores") {
    val prog = "Family A {" +
      "type T = C1 {n: N} | C2 {b: B}" +
      "cases tcase <T> : {} -> {C1: {n: N} -> N, C2: {b: B} -> N, _: {} -> N} = " +
      "lam (x: {}). {C1 = lam (y: {n: N}). 1, C2 = lam (z: {b: B}). 1, _ = lam (w: {}). 0}" +
      "}"
    assert(canParse(pFamDef(Prog), prog))
  }

  // Parsing Families
  test("parse - famdef one type") {
    assert(canParse(
      pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      List("A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        None, 
        Map("T" -> TypeDefn("T", Eq, RecordType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", Eq, Record(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      ))
    ){parseSuccess(pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}")}
  }

  test("parse - famdef extends") {
    val fam = "Family A extends C { type T = {f: B = true, n: N = 3}}"
    val map = "A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")), 
        Map("T" -> TypeDefn("T", Eq, RecordType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", Eq, Record(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      )
    assert(canParse(pFamDef(Prog), fam))
    assertResult(List(map)){parseSuccess(pFamDef(Prog), fam)}
  }

  test("parse - famdef extends and plusEquals, missing defaults") {
    assertThrows[Exception](canParse(
      pFamDef(Prog), "Family A extends C { type T += {f: B, n: N = 3}}"
    ))
  }

  test("parse - famdef extends and plusEquals") {
    val fam = "Family A extends C {type T += {f: B = true, n: N = 3}}"
    val map = "A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")),
        Map("T" -> TypeDefn("T", PlusEq, RecordType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", PlusEq, Record(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      )
    assert(canParse(pFamDef(Prog), fam))
    assertResult(List(map)){parseSuccess(pFamDef(Prog), fam)}
  }

  test("parse - famdef multiple types") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}"
    ))
  }

  test("parse - famdef types + ADTs") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}"
    ))
  }

  test("parse - famdef can parse multiple types and ADTs") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "}"
    ))
  }

  test("parse - famdef can parse types, adts, functions") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "val identity: (B -> B) = lam (x: B). x" +
        "}"
    ))
  }

  test("parse - famdef can parse nested families") {
    val prog =
    """
     |Family A {
     |  Family C {
     |    Family D {}
     |  }
     |  Family E {}
     |}
     |""".stripMargin

    assert(canParse(pFamDef(Prog), prog))
  }

  test("parse - famdef can parse nested families with types, adts, functions") {
    val prog =
    """
      |Family A {
      |  type T = {f: B = true, n: N = 3}
      |  type Weekend = Sat {} | Sun {}
      |  val identity: (B -> B) = lam (x: B). x
      |  Family C {
      |      type R = {s: self(A).T = {}}
      |      type List = Nil {} | Cons {x: N, tail: self(A).List}
      |  }
      |}
      |""".stripMargin

      assert(canParse(pFamDef(Prog), prog))
  }

  // Testing Exceptions
  test("parse - exception: duplicate fields in record") {
    assertThrows[Exception](parse0(pRecType, "{f: N, f: B}"))
  }

  test("parse - exception: duplicate constructors in ADT") {
    assertThrows[Exception](parse0(pAdt, "type T = A {} | A {}"))
  }

  test("parse - exception: duplicate family names") {
    assertThrows[Exception](parse0(pFamDef(Prog), "Family A { Family C {} Family C {} }"))
  }

  test("parse - can parse record fields that are constructors") {
    assert(canParse(pFieldName, "HelloWorld"))
  }

  test("parse - can parse cases by themselves") {
    assert(canParse(pCasesDef, "cases hello_world <T> : {} -> {A: B -> N, C: B -> N} = " +
      "lam (_: {}). {A = lam (x: B). 3, C = lam (x: B). 4}"))
  }

  test("parse - can parse extended definition syntax") {
    assert(canParse(pExtendedDef, """
      def plus(n1: N): Exp -> N =
        case EBase() = n1
        case ENat(n2:N) = n2
      """))
  }

  test("parse - can parse extended definition syntax, empty context") {
    assert(canParse(pExtendedDef, """
      def ev: Exp -> N =
        case EBase() = 0
        case ENat(n:N) = n
      """))
  }

  test("parse - can parse extended definition syntax, currying") {
    assert(canParse(pExp, "foo(a, b, c)(d)"))
  }

  test("parse - can parse extended definition syntax, currying empty") {
    assert(canParse(pExp, "foo()(d)"))
  }

  // replaced strings with ints because we don't have strings right now
  // included built in optionval type
  test("parse - Parse STLCBase and extension (fig 3) in the paper") {
    val fam = 
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
    //print(parse0(pProgram, fam))
    assert(canParse(pProgram, fam))
  }

  /* ============= TEST DEF LINK TO TYP LINK CONVERSION ============= */

  test("parse - linkage def to typ ") {
    val p = "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "val identity: (B -> B) = lam (x: B). x" +
        "}"

    assertResult(parseProgramTypLink(p), ""){
      TypingLinkage(Sp(Prog),None,Map(),Map(), Map(),Map(),Map(),
        Map("A" -> TypingLinkage(Sp(SelfFamily(Sp(Prog),"A")),None,
        Map("T" -> TypeDefn("T",Eq,RecordType(Map("f" -> BType, "n" -> NType))), "R" -> TypeDefn("R",Eq,RecordType(Map("s" -> PathType(Some(Sp(SelfFamily(Sp(Prog),"A"))),"T"))))), 
        Map("T" -> List("f", "n"), "R" -> List("s")), Map("List" -> AdtDefn("List",Eq,Map("Nil" -> RecordType(Map()), "Cons" -> RecordType(Map("x" -> NType, "tail" -> PathType(Some(Sp(SelfFamily(Sp(Prog),"A"))),"List"))))), "Weekend" -> AdtDefn("Weekend",Eq,Map("Sat" -> RecordType(Map()), "Sun" -> RecordType(Map())))),Map("identity" -> FunSig("identity",FunType(BType,BType))),Map(),Map())))
    }
  }

}
