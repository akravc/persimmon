import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestDefParser._
import TestTypParser._
import PrettyPrint._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File

class PersimmonTesting extends AnyFunSuite {
  /* ============= TEST DEF PARSER ============= */

  // Parsing Paths
  test("paths: absolute path") {
    val inp = "A.C.D"
    assert(canParse(TestDefParser.pPath, inp))
    assertResult(
      AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),"D")
    ){parseSuccess(TestDefParser.pPath, inp)}
  }

  test("paths: self prefixed absolute path") {
    val inp = "self(self(A).C).D"
    assert(canParse(TestDefParser.pPath, inp))
    assertResult(
      AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")
    ){parseSuccess(TestDefParser.pPath, inp)}
  }

  test("paths: self path") {
    val inp = "self(self(self(A).C).D)"
    assert(canParse(TestDefParser.pPath, inp))
    assertResult(
      Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))
    ){parseSuccess(TestDefParser.pPath, inp)}
  }

  // Parsing Types
  test("types: nat") {
    assert(canParse(TestDefParser.pType, "N"))
    assertResult(NType){parseSuccess(TestDefParser.pType, "N")}
  }

  test("types: bool") {
    assert(canParse(TestDefParser.pType, "B"))
    assertResult(BType){parseSuccess(TestDefParser.pType, "B")}
  }

  test("types: arrow") {
    assert(canParse(TestDefParser.pType, "B -> N"))
    assertResult(FunType(BType, NType)){parseSuccess(TestDefParser.pType, "B -> N")}
  }

  test("types: absolute path type") {
    assert(canParse(TestDefParser.pType, "A.R"))
    assertResult(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")){parseSuccess(TestDefParser.pType, "A.R")}
  }

  test("types: absolute path path type") {
    val inp = "A.C.D.R"
    assert(canParse(TestDefParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D")), "R")
    ){parseSuccess(TestDefParser.pType, inp)}
  }

  test("types: absolute path self head path type") {
    val inp = "self(self(A).C).D.R"
    assert(canParse(TestDefParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")), "R")
    ) {parseSuccess(TestDefParser.pType, inp)}
  }

  test("types: self path type") {
    assert(canParse(TestDefParser.pType, "self(A).R"))
    assertResult(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "R")){parseSuccess(TestDefParser.pType, "self(A).R")}
  }

  test("types: self path path type") {
    val inp = "self(self(self(A).C).D).R"
    assert(canParse(TestDefParser.pType, inp))
    assertResult(
      PathType(Some(Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))), "R")
    ){parseSuccess(TestDefParser.pType, inp)}
  }

  test("types: record type") {
    assert(canParse(TestDefParser.pType, "{ a: N, b: B, c: A.R }"))
    assertResult(
      RecType(Map("a"->NType, "b"->BType, "c"->PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")))
    ){parseSuccess(TestDefParser.pType, "{ a: N, b: B, c: A.R }")}
  }

  test("types: paren form") {
    assert(canParse(TestDefParser.pType, "(B->{})"))
    assertResult(FunType(BType, RecType(Map()))){parseSuccess(TestDefParser.pType, "(B->{})")}
  }

  // Parsing Expressions
  test("exp: true") {
    assert(canParse(TestDefParser.pExp, "true"))
    assertResult(BExp(true)){parseSuccess(TestDefParser.pExp, "true")}
  }

  test("exp: false") {
    assert(canParse(TestDefParser.pExp, "false"))
    assertResult(BExp(false)){parseSuccess(TestDefParser.pExp, "false")}
  }

  test("exp: nat") {
    assert(canParse(TestDefParser.pExp, "5"))
    assertResult(NExp(5)){parseSuccess(TestDefParser.pExp, "5")}
  }

  test("exp: var") {
    assert(canParse(TestDefParser.pExp, "x"))
    assertResult(Var("x")){parseSuccess(TestDefParser.pExp, "x")}
  }

  test("exp: lam") {
    assert(canParse(TestDefParser.pExp, "lam (x: B). x"))
    assertResult(Lam(Var("x"), BType, Var("x"))){parseSuccess(TestDefParser.pExp, "lam (x: B). x")}
  }

  test("exp: select function from family") {
    assert(canParse(TestDefParser.pExp, "self(A).calculate"))
    assertResult(FamFun(Some(Sp(SelfFamily(Sp(Prog), "A"))), "calculate")){parseSuccess(TestDefParser.pExp, "self(A).calculate")}
  }

  test("exp: app") {
    assert(canParse(TestDefParser.pExp, "(lam (x: B). x) true"))
    assertResult(App(Lam(Var("x"), BType, Var("x")), BExp(true))){parseSuccess(TestDefParser.pExp, "(lam (x: B). x) true")}
    
  }

  test("exp: record") {
    assert(canParse(TestDefParser.pExp, "{ a = 5 , b = true }"))
    assertResult(Rec(Map("a"-> NExp(5), "b" -> BExp(true)))){parseSuccess(TestDefParser.pExp, "{ a = 5, b = true }")}
  }

  test("exp: projection") {
    assert(canParse(TestDefParser.pExp, "{ a = 5 , b = true }.b"))
    assertResult(Proj(Rec(Map("a"-> NExp(5), "b" -> BExp(true))), "b")){parseSuccess(TestDefParser.pExp, "{ a = 5 , b = true }.b")}
  }

  test("exp: instance") {
    assert(canParse(TestDefParser.pExp, "A.R({a = 4})"))
    assertResult(
      Inst(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), Rec(Map("a"->NExp(4))))
    ){parseSuccess(TestDefParser.pExp, "A.R({a = 4})")}
  }

  test("exp: ADT instance") {
    assert(canParse(TestDefParser.pExp, "A.R(C {})"))
    assertResult(
      InstADT(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), "C", Rec(Map()))
    ){parseSuccess(TestDefParser.pExp, "A.R(C {})")}
  }

  test("parser: cases with underscores") {
    val prog = "Family A {" +
      "type T = C1 {n: N} | C2 {b: B}" +
      "cases tcase <T> : {} -> {C1: {n: N} -> N, C2: {b: B} -> N, _: {} -> N} = " +
      "lam (x: {}). {C1 = lam (y: {n: N}). 1, C2 = lam (z: {b: B}). 1, _ = lam (w: {}). 0}" +
      "}"
    assert(canParse(TestDefParser.pFamDef(Prog), prog))
  }

  // Parsing Families
  test("famdef one type") {
    assert(canParse(
      TestDefParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        None, 
        Map("T" -> TypeDefn("T", Eq, RecType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", Eq, Rec(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccess(TestDefParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}")}
  }

  test("famdef extends") {
    val fam = "Family A extends C { type T = {f: B = true, n: N = 3}}"
    val map = "A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")), 
        Map("T" -> TypeDefn("T", Eq, RecType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", Eq, Rec(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      )
    assert(canParse(TestDefParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccess(TestDefParser.pFamDef(Prog), fam)}
  }

  test("famdef extends and plusEquals, missing defaults") {
    assertThrows[Exception](canParse(
      TestDefParser.pFamDef(Prog), "Family A extends C { type T += {f: B, n: N = 3}}"
    ))
  }

  test("famdef extends and plusEquals") {
    val fam = "Family A extends C {type T += {f: B = true, n: N = 3}}"
    val map = "A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")),
        Map("T" -> TypeDefn("T", PlusEq, RecType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", PlusEq, Rec(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      )
    assert(canParse(TestDefParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccess(TestDefParser.pFamDef(Prog), fam)}
  }

  test("famdef multiple types") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}"
    ))
  }

  test("famdef types + ADTs") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}"
    ))
  }

  test("famdef can parse multiple types and ADTs") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "}"
    ))
  }

  test("famdef can parse types, adts, functions") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "val identity: (B -> B) = lam (x: B). x" +
        "}"
    ))
  }

  test("famdef can parse nested families") {
    val prog =
    """
     |Family A {
     |  Family C {
     |    Family D {}
     |  }
     |  Family E {}
     |}
     |""".stripMargin

    assert(canParse(TestDefParser.pFamDef(Prog), prog))
  }

  test("famdef can parse nested families with types, adts, functions") {
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

      assert(canParse(TestDefParser.pFamDef(Prog), prog))
  }

  // Testing Exceptions
  test("exception: duplicate fields in record") {
    assertThrows[Exception](parse0(TestDefParser.pRecType, "{f: N, f: B}"))
  }

  test("exception: duplicate constructors in ADT") {
    assertThrows[Exception](parse0(TestDefParser.pAdt, "type T = A {} | A {}"))
  }

  test("exception: duplicate family names") {
    assertThrows[Exception](parse0(TestDefParser.pFamDef(Prog), "Family A { Family C {} Family C {} }"))
  }

  test("can parse record fields that are constructors") {
    assert(canParse(TestDefParser.pFieldName, "HelloWorld"))
  }

  test("can parse cases by themselves") {
    assert(canParse(TestDefParser.pCasesDef, "cases hello_world <T> : {} -> {A: B -> N, C: B -> N} = " +
      "lam (_: {}). {A = lam (x: B). 3, C = lam (x: B). 4}"))
  }

  test("can parse extended definition syntax") {
    assert(canParse(TestDefParser.pExtendedDef, """
      def plus(n1: N): Exp -> N =
        case EBase() = n1
        case ENat(n2:N) = n2
      """))
  }

  test("can parse extended definition syntax, empty context") {
    assert(canParse(TestDefParser.pExtendedDef, """
      def ev: Exp -> N =
        case EBase() = 0
        case ENat(n:N) = n
      """))
  }

  test("can parse extended definition syntax, currying") {
    assert(canParse(TestDefParser.pExp, "foo(a, b, c)(d)"))
  }

  test("can parse extended definition syntax, currying empty") {
    assert(canParse(TestDefParser.pExp, "foo()(d)"))
  }

  // replaced strings with ints because we don't have strings right now
  // included built in optionval type
  test("Parse STLCBase and extension (fig 3) in the paper") {
    var fam = "Family STLCBase {" +
      " type Ty = TUnit {} | TNat {} | TArr {t1: Ty, t2: Ty}" +
      " type Val = Unit {} | Var {x: N} | Lam {x: N, e: Exp}" +
      " type Exp = EVal {v: Val} | EApp {e1: Exp, e2: Exp}" +
      " type OptionVal = SomeVal {v: Val} | NoneVal {}" +
      " def eval : Exp -> OptionVal = " +
      "  case EVal(v:Val) = SomeVal({v = v})" +
      "  case EApp(e1:Exp, e2:Exp) = if some(eval e1) then " + 
         " ((lam (v: Val). apply(e2) v) (eval e1).v) else NoneVal({})" + 
      " def apply(e2: Exp) : Val -> OptionVal = " +
      "  case Lam(x: N, e: Exp) = eval (subst x e2 e)" + 
      "  case _ = NoneVal({})" + 
      "}" +
      "Family STLCIf extends STLCBase {" +
      " type Ty += TBool {}" +
      " type Val += True {} | False {}" +
      " type Exp += EIf {e: Exp, e1: Exp, e2: Exp}" +
      " def eval: Exp -> OptionVal += " +
      "  case EIf(e:Exp, e1:Exp, e2:Exp) = " +
      "   if some(eval e) then ((lam (v: Val). branch(e1, e2) v) (eval e).v) " +
      "   else NoneVal({})" + 
      " def branch(e1: Exp, e2: Exp): Val -> OptionVal = " +
      "  case True() = eval e1 " +
      "  case False() = eval e2 " +
      "  case _ = NoneVal({})" +
      "}"
    //print(parse0(pProgram, fam))
    assert(canParse(TestDefParser.pProgram, fam))
  }

  /* ============= TEST TYP PARSER ============= */

  // Parsing Paths
  test("typ - paths: absolute path") {
    val inp = "A.C.D"
    assert(canParseTyp(TestTypParser.pPath, inp))
    assertResult(
      AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),"D")
    ){parseSuccessTyp(TestTypParser.pPath, inp)}
  }

  test("typ - paths: self prefixed absolute path") {
    val inp = "self(self(A).C).D"
    assert(canParseTyp(TestTypParser.pPath, inp))
    assertResult(
      AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")
    ){parseSuccessTyp(TestTypParser.pPath, inp)}
  }

  test("typ - paths: self path") {
    val inp = "self(self(self(A).C).D)"
    assert(canParseTyp(TestTypParser.pPath, inp))
    assertResult(
      Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))
    ){parseSuccessTyp(TestTypParser.pPath, inp)}
  }

  // Parsing Types
  test("typ - types: nat") {
    assert(canParseTyp(TestTypParser.pType, "N"))
    assertResult(NType){parseSuccessTyp(TestTypParser.pType, "N")}
  }

  test("typ - types: bool") {
    assert(canParseTyp(TestTypParser.pType, "B"))
    assertResult(BType){parseSuccessTyp(TestTypParser.pType, "B")}
  }

  test("typ - types: arrow") {
    assert(canParseTyp(TestTypParser.pType, "B -> N"))
    assertResult(FunType(BType, NType)){parseSuccessTyp(TestTypParser.pType, "B -> N")}
  }

  test("typ - types: absolute path type") {
    assert(canParseTyp(TestTypParser.pType, "A.R"))
    assertResult(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")){parseSuccessTyp(TestTypParser.pType, "A.R")}
  }

  test("typ - types: absolute path path type") {
    val inp = "A.C.D.R"
    assert(canParseTyp(TestTypParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D")), "R")
    ){parseSuccessTyp(TestTypParser.pType, inp)}
  }

  test("typ - types: absolute path self head path type") {
    val inp = "self(self(A).C).D.R"
    assert(canParseTyp(TestTypParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")), "R")
    ) {parseSuccessTyp(TestTypParser.pType, inp)}
  }

  test("typ - types: self path type") {
    assert(canParseTyp(TestTypParser.pType, "self(A).R"))
    assertResult(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "R")){parseSuccessTyp(TestTypParser.pType, "self(A).R")}
  }

  test("typ - types: self path path type") {
    val inp = "self(self(self(A).C).D).R"
    assert(canParseTyp(TestTypParser.pType, inp))
    assertResult(
      PathType(Some(Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))), "R")
    ){parseSuccessTyp(TestTypParser.pType, inp)}
  }

  test("typ - types: record type") {
    assert(canParseTyp(TestTypParser.pType, "{ a: N, b: B, c: A.R }"))
    assertResult(
      RecType(Map("a"->NType, "b"->BType, "c"->PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")))
    ){parseSuccessTyp(TestTypParser.pType, "{ a: N, b: B, c: A.R }")}
  }

  test("typ - types: paren form") {
    assert(canParseTyp(TestTypParser.pType, "(B->{})"))
    assertResult(FunType(BType, RecType(Map()))){parseSuccessTyp(TestTypParser.pType, "(B->{})")}
  }

  // Parsing Expressions
  test("typ - exp: true") {
    assert(canParseTyp(TestTypParser.pExp, "true"))
    assertResult(BExp(true)){parseSuccessTyp(TestTypParser.pExp, "true")}
  }

  test("typ - exp: false") {
    assert(canParseTyp(TestTypParser.pExp, "false"))
    assertResult(BExp(false)){parseSuccessTyp(TestTypParser.pExp, "false")}
  }

  test("typ - exp: nat") {
    assert(canParseTyp(TestTypParser.pExp, "5"))
    assertResult(NExp(5)){parseSuccessTyp(TestTypParser.pExp, "5")}
  }

  test("typ - exp: var") {
    assert(canParseTyp(TestTypParser.pExp, "x"))
    assertResult(Var("x")){parseSuccessTyp(TestTypParser.pExp, "x")}
  }

  test("typ - exp: lam") {
    assert(canParseTyp(TestTypParser.pExp, "lam (x: B). x"))
    assertResult(Lam(Var("x"), BType, Var("x"))){parseSuccessTyp(TestTypParser.pExp, "lam (x: B). x")}
  }

  test("typ - exp: select function from family") {
    assert(canParseTyp(TestTypParser.pExp, "self(A).calculate"))
    assertResult(FamFun(Some(Sp(SelfFamily(Sp(Prog), "A"))), "calculate")){parseSuccessTyp(TestTypParser.pExp, "self(A).calculate")}
  }

  test("typ - exp: app") {
    assert(canParseTyp(TestTypParser.pExp, "(lam (x: B). x) true"))
    assertResult(App(Lam(Var("x"), BType, Var("x")), BExp(true))){parseSuccessTyp(TestTypParser.pExp, "(lam (x: B). x) true")}
    
  }

  test("typ - exp: record") {
    assert(canParseTyp(TestTypParser.pExp, "{ a = 5 , b = true }"))
    assertResult(Rec(Map("a"-> NExp(5), "b" -> BExp(true)))){parseSuccessTyp(TestTypParser.pExp, "{ a = 5, b = true }")}
  }

  test("typ - exp: projection") {
    assert(canParseTyp(TestTypParser.pExp, "{ a = 5 , b = true }.b"))
    assertResult(Proj(Rec(Map("a"-> NExp(5), "b" -> BExp(true))), "b")){parseSuccessTyp(TestTypParser.pExp, "{ a = 5 , b = true }.b")}
  }

  test("typ - exp: instance") {
    assert(canParseTyp(TestTypParser.pExp, "A.R({a = 4})"))
    assertResult(
      Inst(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), Rec(Map("a"->NExp(4))))
    ){parseSuccessTyp(TestTypParser.pExp, "A.R({a = 4})")}
  }

  test("typ - exp: ADT instance") {
    assert(canParseTyp(TestTypParser.pExp, "A.R(C {})"))
    assertResult(
      InstADT(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), "C", Rec(Map()))
    ){parseSuccessTyp(TestTypParser.pExp, "A.R(C {})")}
  }

  test("typ - parser: cases with underscores") {
    val prog = "Family A {" +
      "type T = C1 {n: N} | C2 {b: B}" +
      "cases tcase <T> : {} -> {C1: {n: N} -> N, C2: {b: B} -> N, _: {} -> N} = " +
      "lam (x: {}). {C1 = lam (y: {n: N}). 1, C2 = lam (z: {b: B}). 1, _ = lam (w: {}). 0}" +
      "}"
    assert(canParseTyp(TestTypParser.pFamDef(Prog), prog))
  }

  // Parsing Families
  test("typ - famdef one type") {
    assert(canParse(
      TestDefParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> TypingLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        None, 
        Map("T" -> TypeDefn("T", Eq, RecType(Map("f"->BType, "n"->NType)))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccessTyp(TestTypParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}")}
  }

  test("typ - famdef extends") {
    val fam = "Family A extends C { type T = {f: B = true, n: N = 3}}"
    val map = "A" -> TypingLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")), 
        Map("T" -> TypeDefn("T", Eq, RecType(Map("f"->BType, "n"->NType)))),
        Map(), Map(), Map(), Map()
      )
    assert(canParseTyp(TestTypParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccessTyp(TestTypParser.pFamDef(Prog), fam)}
  }

  test("typ - famdef extends and plusEquals, missing defaults") {
    assertThrows[Exception](canParse(
      TestDefParser.pFamDef(Prog), "Family A extends C { type T += {f: B, n: N = 3}}"
    ))
  }

  test("typ - famdef extends and plusEquals") {
    val fam = "Family A extends C {type T += {f: B = true, n: N = 3}}"
    val map = "A" -> TypingLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")),
        Map("T" -> TypeDefn("T", PlusEq, RecType(Map("f"->BType, "n"->NType)))),
        Map(), Map(), Map(), Map()
      )
    assert(canParseTyp(TestTypParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccessTyp(TestTypParser.pFamDef(Prog), fam)}
  }

  test("typ - famdef multiple types") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}"
    ))
  }

  test("typ - famdef types + ADTs") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}"
    ))
  }

  test("typ - famdef can parse multiple types and ADTs") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "}"
    ))
  }

  test("typ - famdef can parse types, adts, functions") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "val identity: (B -> B) = lam (x: B). x" +
        "}"
    ))
  }

  test("typ - famdef can parse nested families") {
    val prog =
    """
     |Family A {
     |  Family C {
     |    Family D {}
     |  }
     |  Family E {}
     |}
     |""".stripMargin

    assert(canParseTyp(TestTypParser.pFamDef(Prog), prog))
  }

  test("typ - famdef can parse nested families with types, adts, functions") {
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

      assert(canParseTyp(TestTypParser.pFamDef(Prog), prog))
  }

  // Testing Exceptions
  test("typ - exception: duplicate fields in record") {
    assertThrows[Exception](parse0(TestDefParser.pRecType, "{f: N, f: B}"))
  }

  test("typ - exception: duplicate constructors in ADT") {
    assertThrows[Exception](parse0(TestDefParser.pAdt, "type T = A {} | A {}"))
  }

  test("typ - exception: duplicate family names") {
    assertThrows[Exception](parse0(TestDefParser.pFamDef(Prog), "Family A { Family C {} Family C {} }"))
  }

  test("typ - can parse record fields that are constructors") {
    assert(canParseTyp(TestTypParser.pFieldName, "HelloWorld"))
  }

  test("typ - can parse cases by themselves") {
    assert(canParseTyp(TestTypParser.pCasesDef, "cases hello_world <T> : {} -> {A: B -> N, C: B -> N} = " +
      "lam (_: {}). {A = lam (x: B). 3, C = lam (x: B). 4}"))
  }

  test("typ - can parse extended definition syntax") {
    assert(canParseTyp(TestTypParser.pExtendedDef, """
      def plus(n1: N): Exp -> N =
        case EBase() = n1
        case ENat(n2:N) = n2
      """))
  }

  test("typ - can parse extended definition syntax, empty context") {
    assert(canParseTyp(TestTypParser.pExtendedDef, """
      def ev: Exp -> N =
        case EBase() = 0
        case ENat(n:N) = n
      """))
  }

  test("typ - can parse extended definition syntax, currying") {
    assert(canParseTyp(TestTypParser.pExp, "foo(a, b, c)(d)"))
  }

  test("typ - can parse extended definition syntax, currying empty") {
    assert(canParseTyp(TestTypParser.pExp, "foo()(d)"))
  }

  // replaced strings with ints because we don't have strings right now
  // included built in optionval type
  test("typ - Parse STLCBase and extension (fig 3) in the paper") {
    var fam = "Family STLCBase {" +
      " type Ty = TUnit {} | TNat {} | TArr {t1: Ty, t2: Ty}" +
      " type Val = Unit {} | Var {x: N} | Lam {x: N, e: Exp}" +
      " type Exp = EVal {v: Val} | EApp {e1: Exp, e2: Exp}" +
      " type OptionVal = SomeVal {v: Val} | NoneVal {}" +
      " def eval : Exp -> OptionVal = " +
      "  case EVal(v:Val) = SomeVal({v = v})" +
      "  case EApp(e1:Exp, e2:Exp) = if some(eval e1) then " + 
         " ((lam (v: Val). apply(e2) v) (eval e1).v) else NoneVal({})" + 
      " def apply(e2: Exp) : Val -> OptionVal = " +
      "  case Lam(x: N, e: Exp) = eval (subst x e2 e)" + 
      "  case _ = NoneVal({})" + 
      "}" +
      "Family STLCIf extends STLCBase {" +
      " type Ty += TBool {}" +
      " type Val += True {} | False {}" +
      " type Exp += EIf {e: Exp, e1: Exp, e2: Exp}" +
      " def eval: Exp -> OptionVal += " +
      "  case EIf(e:Exp, e1:Exp, e2:Exp) = " +
      "   if some(eval e) then ((lam (v: Val). branch(e1, e2) v) (eval e).v) " +
      "   else NoneVal({})" + 
      " def branch(e1: Exp, e2: Exp): Val -> OptionVal = " +
      "  case True() = eval e1 " +
      "  case False() = eval e2 " +
      "  case _ = NoneVal({})" +
      "}"
    //print(parse0(pProgram, fam))
    assert(canParseTyp(TestTypParser.pProgram, fam))
  }

}