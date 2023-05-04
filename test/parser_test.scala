import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import PersimmonLinkages._
import TestDefParser._
import TestTypParser._
import PrettyPrint._
import scala.language.postfixOps
import java.io.PrintWriter
import java.io.File
import scala.io.Source

class ParserTesting extends AnyFunSuite {

  def readFile(filename: String): String = { 
    return Source.fromFile(filename).getLines.mkString
  }


  /* ============= TEST RES EXAMPLES ============= */

  test("parse - ex: ab") {
    val p = readFile("res/ab")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcode") {
    val p = readFile("res/abcode")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodeover") {
    val p = readFile("res/abcodeover")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodeovern") {
    val p = readFile("res/abcodeovern")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodepaper") {
    val p = readFile("res/abcodepaper")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodepaper2") {
    val p = readFile("res/abcodepaper2")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodepaper2sugar") {
    val p = readFile("res/abcodepaper2sugar")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodepaper2sugar2") {
    val p = readFile("res/abcodepaper2sugar2")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: abcodepaper2sugar2b") {
    val p = readFile("res/abcodepaper2sugar2b")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: default") {
    val p = readFile("res/default")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: even_odd") {
    val p = readFile("res/even_odd")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: example") {
    val p = readFile("res/example")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: mixins") {
    val p = readFile("res/mixins")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: mixins0") {
    val p = readFile("res/mixins0")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: mixins00") {
    val p = readFile("res/mixins00")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: mixins1") {
    val p = readFile("res/mixins1")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: mixins2") {
    val p = readFile("res/mixins2")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: pretty_example") {
    val p = readFile("res/pretty_example")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: reso") {
    val p = readFile("res/reso")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: stlc") {
    val p = readFile("res/stlc")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test1") {
    val p = readFile("res/test1")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test1b") {
    val p = readFile("res/test1b")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test2") {
    val p = readFile("res/test2")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test3") {
    val p = readFile("res/test3")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test4") {
    val p = readFile("res/test4")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test5") {
    val p = readFile("res/test5")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: test6") {
    val p = readFile("res/test6")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: triple") {
    val p = readFile("res/triple")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: wrapper") {
    val p = readFile("res/wrapper")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: wrapper2") {
    val p = readFile("res/wrapper2")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: wrapper3") {
    val p = readFile("res/wrapper3")
    assert(canParse(TestDefParser.pProgram, p))
  }
  test("parse - ex: wrapper4") {
    val p = readFile("res/wrapper4")
    assert(canParse(TestDefParser.pProgram, p))
  }


  /* ============= TEST DEF PARSER ============= */

  // Parsing Paths
  test("parse - paths: absolute path") {
    val inp = "A.C.D"
    assert(canParse(TestDefParser.pPath, inp))
    assertResult(
      AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),"D")
    ){parseSuccess(TestDefParser.pPath, inp)}
  }

  test("parse - paths: self prefixed absolute path") {
    val inp = "self(self(A).C).D"
    assert(canParse(TestDefParser.pPath, inp))
    assertResult(
      AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")
    ){parseSuccess(TestDefParser.pPath, inp)}
  }

  test("parse - paths: self path") {
    val inp = "self(self(self(A).C).D)"
    assert(canParse(TestDefParser.pPath, inp))
    assertResult(
      Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))
    ){parseSuccess(TestDefParser.pPath, inp)}
  }

  // Parsing Types
  test("parse - types: nat") {
    assert(canParse(TestDefParser.pType, "N"))
    assertResult(NType){parseSuccess(TestDefParser.pType, "N")}
  }

  test("parse - types: bool") {
    assert(canParse(TestDefParser.pType, "B"))
    assertResult(BType){parseSuccess(TestDefParser.pType, "B")}
  }

  test("parse - types: arrow") {
    assert(canParse(TestDefParser.pType, "B -> N"))
    assertResult(FunType(BType, NType)){parseSuccess(TestDefParser.pType, "B -> N")}
  }

  test("parse - types: absolute path type") {
    assert(canParse(TestDefParser.pType, "A.R"))
    assertResult(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")){parseSuccess(TestDefParser.pType, "A.R")}
  }

  test("parse - types: absolute path path type") {
    val inp = "A.C.D.R"
    assert(canParse(TestDefParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D")), "R")
    ){parseSuccess(TestDefParser.pType, inp)}
  }

  test("parse - types: absolute path self head path type") {
    val inp = "self(self(A).C).D.R"
    assert(canParse(TestDefParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")), "R")
    ) {parseSuccess(TestDefParser.pType, inp)}
  }

  test("parse - types: self path type") {
    assert(canParse(TestDefParser.pType, "self(A).R"))
    assertResult(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "R")){parseSuccess(TestDefParser.pType, "self(A).R")}
  }

  test("parse - types: self path path type") {
    val inp = "self(self(self(A).C).D).R"
    assert(canParse(TestDefParser.pType, inp))
    assertResult(
      PathType(Some(Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))), "R")
    ){parseSuccess(TestDefParser.pType, inp)}
  }

  test("parse - types: record type") {
    assert(canParse(TestDefParser.pType, "{ a: N, b: B, c: A.R }"))
    assertResult(
      RecordType(Map("a"->NType, "b"->BType, "c"->PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")))
    ){parseSuccess(TestDefParser.pType, "{ a: N, b: B, c: A.R }")}
  }

  test("parse - types: paren form") {
    assert(canParse(TestDefParser.pType, "(B->{})"))
    assertResult(FunType(BType, RecordType(Map()))){parseSuccess(TestDefParser.pType, "(B->{})")}
  }

  // Parsing Expressions
  test("parse - exp: true") {
    assert(canParse(TestDefParser.pExp, "true"))
    assertResult(BExp(true)){parseSuccess(TestDefParser.pExp, "true")}
  }

  test("parse - exp: false") {
    assert(canParse(TestDefParser.pExp, "false"))
    assertResult(BExp(false)){parseSuccess(TestDefParser.pExp, "false")}
  }

  test("parse - exp: nat") {
    assert(canParse(TestDefParser.pExp, "5"))
    assertResult(NExp(5)){parseSuccess(TestDefParser.pExp, "5")}
  }

  test("parse - exp: var") {
    assert(canParse(TestDefParser.pExp, "x"))
    assertResult(Var("x")){parseSuccess(TestDefParser.pExp, "x")}
  }

  test("parse - exp: lam") {
    assert(canParse(TestDefParser.pExp, "lam (x: B). x"))
    assertResult(Lam(Var("x"), BType, Var("x"))){parseSuccess(TestDefParser.pExp, "lam (x: B). x")}
  }

  test("parse - exp: select function from family") {
    assert(canParse(TestDefParser.pExp, "self(A).calculate"))
    assertResult(FamFun(Some(Sp(SelfFamily(Sp(Prog), "A"))), "calculate")){parseSuccess(TestDefParser.pExp, "self(A).calculate")}
  }

  test("parse - exp: app") {
    assert(canParse(TestDefParser.pExp, "(lam (x: B). x) true"))
    assertResult(App(Lam(Var("x"), BType, Var("x")), BExp(true))){parseSuccess(TestDefParser.pExp, "(lam (x: B). x) true")}
    
  }

  test("parse - exp: record") {
    assert(canParse(TestDefParser.pExp, "{ a = 5 , b = true }"))
    assertResult(Record(Map("a"-> NExp(5), "b" -> BExp(true)))){parseSuccess(TestDefParser.pExp, "{ a = 5, b = true }")}
  }

  test("parse - exp: projection") {
    assert(canParse(TestDefParser.pExp, "{ a = 5 , b = true }.b"))
    assertResult(Proj(Record(Map("a"-> NExp(5), "b" -> BExp(true))), "b")){parseSuccess(TestDefParser.pExp, "{ a = 5 , b = true }.b")}
  }

  test("parse - exp: instance") {
    assert(canParse(TestDefParser.pExp, "A.R({a = 4})"))
    assertResult(
      Inst(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), Record(Map("a"->NExp(4))))
    ){parseSuccess(TestDefParser.pExp, "A.R({a = 4})")}
  }

  test("parse - exp: ADT instance") {
    assert(canParse(TestDefParser.pExp, "A.R(C {})"))
    assertResult(
      InstADT(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), "C", Record(Map()))
    ){parseSuccess(TestDefParser.pExp, "A.R(C {})")}
  }

  test("parse - parser: cases with underscores") {
    val prog = "Family A {" +
      "type T = C1 {n: N} | C2 {b: B}" +
      "cases tcase <T> : {} -> {C1: {n: N} -> N, C2: {b: B} -> N, _: {} -> N} = " +
      "lam (x: {}). {C1 = lam (y: {n: N}). 1, C2 = lam (z: {b: B}). 1, _ = lam (w: {}). 0}" +
      "}"
    assert(canParse(TestDefParser.pFamDef(Prog), prog))
  }

  // Parsing Families
  test("parse - famdef one type") {
    assert(canParse(
      TestDefParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> DefinitionLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        None, 
        Map("T" -> TypeDefn("T", Eq, RecordType(Map("f"->BType, "n"->NType)))),
        Map("T" -> DefaultDefn("T", Eq, Record(Map("f"->BExp(true), "n"->NExp(3))))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccess(TestDefParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}")}
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
    assert(canParse(TestDefParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccess(TestDefParser.pFamDef(Prog), fam)}
  }

  test("parse - famdef extends and plusEquals, missing defaults") {
    assertThrows[Exception](canParse(
      TestDefParser.pFamDef(Prog), "Family A extends C { type T += {f: B, n: N = 3}}"
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
    assert(canParse(TestDefParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccess(TestDefParser.pFamDef(Prog), fam)}
  }

  test("parse - famdef multiple types") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}"
    ))
  }

  test("parse - famdef types + ADTs") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}"
    ))
  }

  test("parse - famdef can parse multiple types and ADTs") {
    assert(canParse(TestDefParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "}"
    ))
  }

  test("parse - famdef can parse types, adts, functions") {
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

    assert(canParse(TestDefParser.pFamDef(Prog), prog))
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

      assert(canParse(TestDefParser.pFamDef(Prog), prog))
  }

  // Testing Exceptions
  test("parse - exception: duplicate fields in record") {
    assertThrows[Exception](parse0(TestDefParser.pRecType, "{f: N, f: B}"))
  }

  test("parse - exception: duplicate constructors in ADT") {
    assertThrows[Exception](parse0(TestDefParser.pAdt, "type T = A {} | A {}"))
  }

  test("parse - exception: duplicate family names") {
    assertThrows[Exception](parse0(TestDefParser.pFamDef(Prog), "Family A { Family C {} Family C {} }"))
  }

  test("parse - can parse record fields that are constructors") {
    assert(canParse(TestDefParser.pFieldName, "HelloWorld"))
  }

  test("parse - can parse cases by themselves") {
    assert(canParse(TestDefParser.pCasesDef, "cases hello_world <T> : {} -> {A: B -> N, C: B -> N} = " +
      "lam (_: {}). {A = lam (x: B). 3, C = lam (x: B). 4}"))
  }

  test("parse - can parse extended definition syntax") {
    assert(canParse(TestDefParser.pExtendedDef, """
      def plus(n1: N): Exp -> N =
        case EBase() = n1
        case ENat(n2:N) = n2
      """))
  }

  test("parse - can parse extended definition syntax, empty context") {
    assert(canParse(TestDefParser.pExtendedDef, """
      def ev: Exp -> N =
        case EBase() = 0
        case ENat(n:N) = n
      """))
  }

  test("parse - can parse extended definition syntax, currying") {
    assert(canParse(TestDefParser.pExp, "foo(a, b, c)(d)"))
  }

  test("parse - can parse extended definition syntax, currying empty") {
    assert(canParse(TestDefParser.pExp, "foo()(d)"))
  }

  // replaced strings with ints because we don't have strings right now
  // included built in optionval type
  test("parse - Parse STLCBase and extension (fig 3) in the paper") {
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
    //print(parse0(pProgram, fam))
    assert(canParse(TestDefParser.pProgram, fam))
  }

  /* ============= TEST TYP PARSER ============= */

  // Parsing Paths
  test("parse - typ - paths: absolute path") {
    val inp = "A.C.D"
    assert(canParseTyp(TestTypParser.pPath, inp))
    assertResult(
      AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),"D")
    ){parseSuccessTyp(TestTypParser.pPath, inp)}
  }

  test("parse - typ - paths: self prefixed absolute path") {
    val inp = "self(self(A).C).D"
    assert(canParseTyp(TestTypParser.pPath, inp))
    assertResult(
      AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")
    ){parseSuccessTyp(TestTypParser.pPath, inp)}
  }

  test("parse - typ - paths: self path") {
    val inp = "self(self(self(A).C).D)"
    assert(canParseTyp(TestTypParser.pPath, inp))
    assertResult(
      Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))
    ){parseSuccessTyp(TestTypParser.pPath, inp)}
  }

  // Parsing Types
  test("parse - typ - types: nat") {
    assert(canParseTyp(TestTypParser.pType, "N"))
    assertResult(NType){parseSuccessTyp(TestTypParser.pType, "N")}
  }

  test("parse - typ - types: bool") {
    assert(canParseTyp(TestTypParser.pType, "B"))
    assertResult(BType){parseSuccessTyp(TestTypParser.pType, "B")}
  }

  test("parse - typ - types: arrow") {
    assert(canParseTyp(TestTypParser.pType, "B -> N"))
    assertResult(FunType(BType, NType)){parseSuccessTyp(TestTypParser.pType, "B -> N")}
  }

  test("parse - typ - types: absolute path type") {
    assert(canParseTyp(TestTypParser.pType, "A.R"))
    assertResult(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")){parseSuccessTyp(TestTypParser.pType, "A.R")}
  }

  test("parse - typ - types: absolute path path type") {
    val inp = "A.C.D.R"
    assert(canParseTyp(TestTypParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D")), "R")
    ){parseSuccessTyp(TestTypParser.pType, inp)}
  }

  test("parse - typ - types: absolute path self head path type") {
    val inp = "self(self(A).C).D.R"
    assert(canParseTyp(TestTypParser.pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")), "R")
    ) {parseSuccessTyp(TestTypParser.pType, inp)}
  }

  test("parse - typ - types: self path type") {
    assert(canParseTyp(TestTypParser.pType, "self(A).R"))
    assertResult(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "R")){parseSuccessTyp(TestTypParser.pType, "self(A).R")}
  }

  test("parse - typ - types: self path path type") {
    val inp = "self(self(self(A).C).D).R"
    assert(canParseTyp(TestTypParser.pType, inp))
    assertResult(
      PathType(Some(Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))), "R")
    ){parseSuccessTyp(TestTypParser.pType, inp)}
  }

  test("parse - typ - types: record type") {
    assert(canParseTyp(TestTypParser.pType, "{ a: N, b: B, c: A.R }"))
    assertResult(
      RecordType(Map("a"->NType, "b"->BType, "c"->PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")))
    ){parseSuccessTyp(TestTypParser.pType, "{ a: N, b: B, c: A.R }")}
  }

  test("parse - typ - types: paren form") {
    assert(canParseTyp(TestTypParser.pType, "(B->{})"))
    assertResult(FunType(BType, RecordType(Map()))){parseSuccessTyp(TestTypParser.pType, "(B->{})")}
  }

  // Parsing Expressions
  test("parse - typ - exp: true") {
    assert(canParseTyp(TestTypParser.pExp, "true"))
    assertResult(BExp(true)){parseSuccessTyp(TestTypParser.pExp, "true")}
  }

  test("parse - typ - exp: false") {
    assert(canParseTyp(TestTypParser.pExp, "false"))
    assertResult(BExp(false)){parseSuccessTyp(TestTypParser.pExp, "false")}
  }

  test("parse - typ - exp: nat") {
    assert(canParseTyp(TestTypParser.pExp, "5"))
    assertResult(NExp(5)){parseSuccessTyp(TestTypParser.pExp, "5")}
  }

  test("parse - typ - exp: var") {
    assert(canParseTyp(TestTypParser.pExp, "x"))
    assertResult(Var("x")){parseSuccessTyp(TestTypParser.pExp, "x")}
  }

  test("parse - typ - exp: lam") {
    assert(canParseTyp(TestTypParser.pExp, "lam (x: B). x"))
    assertResult(Lam(Var("x"), BType, Var("x"))){parseSuccessTyp(TestTypParser.pExp, "lam (x: B). x")}
  }

  test("parse - typ - exp: select function from family") {
    assert(canParseTyp(TestTypParser.pExp, "self(A).calculate"))
    assertResult(FamFun(Some(Sp(SelfFamily(Sp(Prog), "A"))), "calculate")){parseSuccessTyp(TestTypParser.pExp, "self(A).calculate")}
  }

  test("parse - typ - exp: app") {
    assert(canParseTyp(TestTypParser.pExp, "(lam (x: B). x) true"))
    assertResult(App(Lam(Var("x"), BType, Var("x")), BExp(true))){parseSuccessTyp(TestTypParser.pExp, "(lam (x: B). x) true")}
    
  }

  test("parse - typ - exp: record") {
    assert(canParseTyp(TestTypParser.pExp, "{ a = 5 , b = true }"))
    assertResult(Record(Map("a"-> NExp(5), "b" -> BExp(true)))){parseSuccessTyp(TestTypParser.pExp, "{ a = 5, b = true }")}
  }

  test("parse - typ - exp: projection") {
    assert(canParseTyp(TestTypParser.pExp, "{ a = 5 , b = true }.b"))
    assertResult(Proj(Record(Map("a"-> NExp(5), "b" -> BExp(true))), "b")){parseSuccessTyp(TestTypParser.pExp, "{ a = 5 , b = true }.b")}
  }

  test("parse - typ - exp: instance") {
    assert(canParseTyp(TestTypParser.pExp, "A.R({a = 4})"))
    assertResult(
      Inst(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), Record(Map("a"->NExp(4))))
    ){parseSuccessTyp(TestTypParser.pExp, "A.R({a = 4})")}
  }

  test("parse - typ - exp: ADT instance") {
    assert(canParseTyp(TestTypParser.pExp, "A.R(C {})"))
    assertResult(
      InstADT(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), "C", Record(Map()))
    ){parseSuccessTyp(TestTypParser.pExp, "A.R(C {})")}
  }

  test("parse - typ - parser: cases with underscores") {
    val prog = "Family A {" +
      "type T = C1 {n: N} | C2 {b: B}" +
      "cases tcase <T> : {} -> {C1: {n: N} -> N, C2: {b: B} -> N, _: {} -> N} = " +
      "lam (x: {}). {C1 = lam (y: {n: N}). 1, C2 = lam (z: {b: B}). 1, _ = lam (w: {}). 0}" +
      "}"
    assert(canParseTyp(TestTypParser.pFamDef(Prog), prog))
  }

  // Parsing Families
  test("parse - typ - famdef one type") {
    assert(canParse(
      TestDefParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> TypingLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        None, 
        Map("T" -> TypeDefn("T", Eq, RecordType(Map("f"->BType, "n"->NType)))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccessTyp(TestTypParser.pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}")}
  }

  test("parse - typ - famdef extends") {
    val fam = "Family A extends C { type T = {f: B = true, n: N = 3}}"
    val map = "A" -> TypingLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")), 
        Map("T" -> TypeDefn("T", Eq, RecordType(Map("f"->BType, "n"->NType)))),
        Map(), Map(), Map(), Map()
      )
    assert(canParseTyp(TestTypParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccessTyp(TestTypParser.pFamDef(Prog), fam)}
  }

  test("parse - typ - famdef extends and plusEquals, missing defaults") {
    assertThrows[Exception](canParse(
      TestDefParser.pFamDef(Prog), "Family A extends C { type T += {f: B, n: N = 3}}"
    ))
  }

  test("parse - typ - famdef extends and plusEquals") {
    val fam = "Family A extends C {type T += {f: B = true, n: N = 3}}"
    val map = "A" -> TypingLinkage(
        Sp(SelfFamily(Sp(Prog), "A")),
        Some(AbsoluteFamily(Sp(Prog), "C")),
        Map("T" -> TypeDefn("T", PlusEq, RecordType(Map("f"->BType, "n"->NType)))),
        Map(), Map(), Map(), Map()
      )
    assert(canParseTyp(TestTypParser.pFamDef(Prog), fam))
    assertResult(map){parseSuccessTyp(TestTypParser.pFamDef(Prog), fam)}
  }

  test("parse - typ - famdef multiple types") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}"
    ))
  }

  test("parse - typ - famdef types + ADTs") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}"
    ))
  }

  test("parse - typ - famdef can parse multiple types and ADTs") {
    assert(canParseTyp(TestTypParser.pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "}"
    ))
  }

  test("parse - typ - famdef can parse types, adts, functions") {
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

  test("parse - typ - famdef can parse nested families") {
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

  test("parse - typ - famdef can parse nested families with types, adts, functions") {
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
  test("parse - typ - exception: duplicate fields in record") {
    assertThrows[Exception](parse0(TestDefParser.pRecType, "{f: N, f: B}"))
  }

  test("parse - typ - exception: duplicate constructors in ADT") {
    assertThrows[Exception](parse0(TestDefParser.pAdt, "type T = A {} | A {}"))
  }

  test("parse - typ - exception: duplicate family names") {
    assertThrows[Exception](parse0(TestDefParser.pFamDef(Prog), "Family A { Family C {} Family C {} }"))
  }

  test("parse - typ - can parse record fields that are constructors") {
    assert(canParseTyp(TestTypParser.pFieldName, "HelloWorld"))
  }

  test("parse - typ - can parse cases by themselves") {
    assert(canParseTyp(TestTypParser.pCasesDef, "cases hello_world <T> : {} -> {A: B -> N, C: B -> N} = " +
      "lam (_: {}). {A = lam (x: B). 3, C = lam (x: B). 4}"))
  }

  test("parse - typ - can parse extended definition syntax") {
    assert(canParseTyp(TestTypParser.pExtendedDef, """
      def plus(n1: N): Exp -> N =
        case EBase() = n1
        case ENat(n2:N) = n2
      """))
  }

  test("parse - typ - can parse extended definition syntax, empty context") {
    assert(canParseTyp(TestTypParser.pExtendedDef, """
      def ev: Exp -> N =
        case EBase() = 0
        case ENat(n:N) = n
      """))
  }

  test("parse - typ - can parse extended definition syntax, currying") {
    assert(canParseTyp(TestTypParser.pExp, "foo(a, b, c)(d)"))
  }

  test("parse - typ - can parse extended definition syntax, currying empty") {
    assert(canParseTyp(TestTypParser.pExp, "foo()(d)"))
  }

  // replaced strings with ints because we don't have strings right now
  // included built in optionval type
  test("parse - typ - Parse STLCBase and extension (fig 3) in the paper") {
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
    //print(parse0(pProgram, fam))
    assert(canParseTyp(TestTypParser.pProgram, fam))
  }

}