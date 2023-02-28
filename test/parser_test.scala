import org.scalatest.funsuite.AnyFunSuite
import PersimmonSyntax._
import TestParser._
import scala.language.postfixOps

class ParserTesting extends AnyFunSuite {
  /* ==================================== PARSER TESTING ==================================== */

  // Parsing Paths
  test("paths: absolute path") {
    val inp = "A.C.D"
    assert(canParse(pPath, inp))
    assertResult(
      AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),"D")
    ){parseSuccess(pPath, inp)}
  }

  test("paths: self prefixed absolute path") {
    val inp = "self(self(A).C).D"
    assert(canParse(pPath, inp))
    assertResult(
      AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")
    ){parseSuccess(pPath, inp)}
  }

  test("paths: self path") {
    val inp = "self(self(self(A).C).D)"
    assert(canParse(pPath, inp))
    assertResult(
      Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))
    ){parseSuccess(pPath, inp)}
  }

  // Parsing Types
  test("types: nat") {
    assert(canParse(pType, "N"))
    assertResult(NType){parseSuccess(pType, "N")}
  }

  test("types: bool") {
    assert(canParse(pType, "B"))
    assertResult(BType){parseSuccess(pType, "B")}
  }

  test("types: arrow") {
    assert(canParse(pType, "B -> N"))
    assertResult(FunType(BType, NType)){parseSuccess(pType, "B -> N")}
  }

  test("types: absolute path type") {
    assert(canParse(pType, "A.R"))
    assertResult(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")){parseSuccess(pType, "A.R")}
  }

  test("types: absolute path path type") {
    val inp = "A.C.D.R"
    assert(canParse(pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D")), "R")
    ){parseSuccess(pType, inp)}
  }

  test("types: absolute path self head path type") {
    val inp = "self(self(A).C).D.R"
    assert(canParse(pType, inp))
    assertResult(
      PathType(Some(AbsoluteFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D")), "R")
    ) {parseSuccess(pType, inp)}
  }

  test("types: self path type") {
    assert(canParse(pType, "self(A).R"))
    assertResult(PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "R")){parseSuccess(pType, "self(A).R")}
  }

  test("types: self path path type") {
    val inp = "self(self(self(A).C).D).R"
    assert(canParse(pType, inp))
    assertResult(
      PathType(Some(Sp(SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"))), "R")
    ){parseSuccess(pType, inp)}
  }

  test("types: record type") {
    assert(canParse(pType, "{ a: N, b: B, c: A.R }"))
    assertResult(
      RecType(Map("a"->NType, "b"->BType, "c"->PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R")))
    ){parseSuccess(pType, "{ a: N, b: B, c: A.R }")}
  }

  test("types: paren form") {
    assert(canParse(pType, "(B->{})"))
    assertResult(FunType(BType, RecType(Map()))){parseSuccess(pType, "(B->{})")}
  }

  // Parsing Expressions
  test("exp: true") {
    assert(canParse(pExp, "true"))
    assertResult(BExp(true)){parseSuccess(pExp, "true")}
  }

  test("exp: false") {
    assert(canParse(pExp, "false"))
    assertResult(BExp(false)){parseSuccess(pExp, "false")}
  }

  test("exp: nat") {
    assert(canParse(pExp, "5"))
    assertResult(NExp(5)){parseSuccess(pExp, "5")}
  }

  test("exp: var") {
    assert(canParse(pExp, "x"))
    assertResult(Var("x")){parseSuccess(pExp, "x")}
  }

  test("exp: lam") {
    assert(canParse(pExp, "lam (x: B). x"))
    assertResult(Lam(Var("x"), BType, Var("x"))){parseSuccess(pExp, "lam (x: B). x")}
  }

  test("exp: select function from family") {
    assert(canParse(pExp, "self(A).calculate"))
    assertResult(FamFun(Some(Sp(SelfFamily(Sp(Prog), "A"))), "calculate")){parseSuccess(pExp, "self(A).calculate")}
  }

  test("exp: app") {
    assert(canParse(pExp, "(lam (x: B). x) true"))
    assertResult(App(Lam(Var("x"), BType, Var("x")), BExp(true))){parseSuccess(pExp, "(lam (x: B). x) true")}
    
  }

  test("exp: record") {
    assert(canParse(pExp, "{ a = 5 , b = true }"))
    assertResult(Rec(Map("a"-> NExp(5), "b" -> BExp(true)))){parseSuccess(pExp, "{ a = 5, b = true }")}
  }

  test("exp: projection") {
    assert(canParse(pExp, "{ a = 5 , b = true }.b"))
    assertResult(Proj(Rec(Map("a"-> NExp(5), "b" -> BExp(true))), "b")){parseSuccess(pExp, "{ a = 5 , b = true }.b")}
  }

  test("exp: instance") {
    assert(canParse(pExp, "A.R({a = 4})"))
    assertResult(
      Inst(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), Rec(Map("a"->NExp(4))))
    ){parseSuccess(pExp, "A.R({a = 4})")}
  }

  test("exp: ADT instance") {
    assert(canParse(pExp, "A.R(C {})"))
    assertResult(
      InstADT(PathType(Some(AbsoluteFamily(Sp(Prog), "A")), "R"), "C", Rec(Map()))
    ){parseSuccess(pExp, "A.R(C {})")}
  }

  test("parser: cases with underscores") {
    val prog = "Family A {" +
      "type T = C1 {n: N} | C2 {b: B}" +
      "cases tcase <T> : {} -> {C1: {n: N} -> N, C2: {b: B} -> N, _: {} -> N} = " +
      "lam (x: {}). {C1 = lam (y: {n: N}). 1, C2 = lam (z: {b: B}). 1, _ = lam (w: {}). 0}" +
      "}"
    assert(canParse(pFamDef(Prog), prog))
  }

  // Parsing Families
  test("famdef one type") {
    assert(canParse(
      pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> Linkage(
        AbsoluteFamily(Sp(Prog), "A"),
        SelfFamily(Sp(Prog), "A"),
        None,
        Map("T" -> TypeDefn("T", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None))),
        Map("T" -> DefaultDefn("T", Eq, DefnBody(Some(Rec(Map("f"->BExp(true), "n"->NExp(3)))), None, None))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccess(pFamDef(Prog), "Family A { type T = {f: B = true, n: N = 3}}")}
  }

  test("famdef extends") {
    assert(canParse(
      pFamDef(Prog), "Family A extends C { type T = {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> Linkage(
        AbsoluteFamily(Sp(Prog), "A"),
        SelfFamily(Sp(Prog), "A"),
        Some(AbsoluteFamily(Sp(Prog), "C")),
        Map("T" -> TypeDefn("T", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None))),
        Map("T" -> DefaultDefn("T", Eq, DefnBody(Some(Rec(Map("f"->BExp(true), "n"->NExp(3)))), None, None))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccess(pFamDef(Prog), "Family A extends C { type T = {f: B = true, n: N = 3}}")}
  }

  test("famdef extends and plusEquals, missing defaults") {
    assertThrows[Exception](canParse(
      pFamDef(Prog), "Family A extends C { type T += {f: B, n: N = 3}}"
    ))
  }

  test("famdef extends and plusEquals") {
    assert(canParse(
      pFamDef(Prog), "Family A extends C {type T += {f: B = true, n: N = 3}}"
    ))
    assertResult(
      "A" -> Linkage(
        AbsoluteFamily(Sp(Prog), "A"),
        SelfFamily(Sp(Prog), "A"),
        Some(AbsoluteFamily(Sp(Prog), "C")),
        Map("T" -> TypeDefn("T", PlusEq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None))),
        Map("T" -> DefaultDefn("T", PlusEq, DefnBody(Some(Rec(Map("f"->BExp(true), "n"->NExp(3)))), None, None))),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccess(pFamDef(Prog), "Family A extends C { type T += {f: B = true, n: N = 3}}")}
  }

  test("famdef multiple types") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}"
    ))
    assertResult(
      "A" -> Linkage(
        AbsoluteFamily(Sp(Prog), "A"),
        SelfFamily(Sp(Prog), "A"),
        None,
        Map(
          "T" -> TypeDefn("T", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None)),
          "R" -> TypeDefn("R", Eq, DefnBody(Some(RecType(Map("s"->PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T")))), None, None))
        ),
        Map(
          "T" -> DefaultDefn("T", Eq, DefnBody(Some(Rec(Map("f"->BExp(true), "n"->NExp(3)))), None, None)),
          "R" -> DefaultDefn("R", Eq, DefnBody(Some(Rec(Map("s"->Rec(Map())))), None, None))
        ),
        Map(), Map(), Map(), Map()
      )
    ){parseSuccess(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "}")}
  }

  test("famdef types + ADTs") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}"
    ))
    assertResult(
      "A" -> Linkage(
        AbsoluteFamily(Sp(Prog), "A"),
        SelfFamily(Sp(Prog), "A"),
        None,
        // types
        Map(
          "T" -> TypeDefn("T", Eq, DefnBody(Some(RecType(Map("f"->BType, "n"->NType))), None, None)),
          "R" -> TypeDefn("R", Eq, DefnBody(Some(RecType(Map("s"->PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "T")))), None, None))
        ),
        // defaults
        Map(
          "T" -> DefaultDefn("T", Eq, DefnBody(Some(Rec(Map("f"->BExp(true), "n"->NExp(3)))), None, None)),
          "R" -> DefaultDefn("R", Eq, DefnBody(Some(Rec(Map("s"->Rec(Map())))), None, None))
        ),
        // adts
        Map(
          "List"-> AdtDefn(
            "List", Eq, DefnBody(
              Some(Map(
                "Nil" -> RecType(Map()),
                "Cons" -> RecType(Map(
                  "x" -> NType,
                  "tail" -> PathType(Some(Sp(SelfFamily(Sp(Prog), "A"))), "List")
                ))
              )),
              None, None
            )
          )
        ),
        Map(), Map(), Map()
      )
    ){parseSuccess(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "}")}
  }

  test("famdef can parse multiple types and ADTs") {
    assert(canParse(pFamDef(Prog),
      "Family A { " +
        "type T = {f: B = true, n: N = 3} " +
        "type R = {s: self(A).T = {}}" +
        "type List = Nil {} | Cons {x: N, tail: self(A).List}" +
        "type Weekend = Sat {} | Sun {}" +
        "}"
    ))
  }

  test("famdef can parse types, adts, functions") {
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

    assert(canParse(pFamDef(Prog), prog))

    assertResult(
      "A" -> Linkage(
        AbsoluteFamily(Sp(Prog), "A"),
        SelfFamily(Sp(Prog), "A"),
        None,
        Map(), Map(), Map(), Map(), Map(),
        Map(
          "C" -> Linkage(
            AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"),
            SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C"),
            None,
            Map(), Map(), Map(), Map(), Map(),
            Map(
              "D" -> Linkage(
                AbsoluteFamily(AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "C"), "D"),
                SelfFamily(Sp(SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "C")), "D"),
                None,
                Map(), Map(), Map(), Map(), Map(), Map()
              )
            )
          ),
          "E" -> Linkage(
            AbsoluteFamily(AbsoluteFamily(Sp(Prog), "A"), "E"),
            SelfFamily(Sp(SelfFamily(Sp(Prog), "A")), "E"),
            None,
            Map(), Map(), Map(), Map(), Map(), Map()
          )
        )
      )
    ){parseSuccess(pFamDef(Prog), prog)}
  }

  test("famdef can parse nested families with types, adts, functions") {
    """
      |Family A {
      |  type T = {f: B = true, n: N = 3}
      |  type Weekend = Sat {} | Sun {}
      |  val identity: (B -> B) = lam (x: B). x
      |  Family C {
      |      type R = {s: self(A).T = {}}
      |      type List = Nil {} | Cons {x: N, tail: self(A).List}
      |  }
      |  Family D {
      |    val add1: (N -> N) = lam (x: N). x + 1
      |  }
      |}
      |""".stripMargin
  }

  // Testing Exceptions
  test("exception: duplicate fields in record") {
    assertThrows[Exception](parse0(pRecType, "{f: N, f: B}"))
  }

  test("exception: duplicate constructors in ADT") {
    assertThrows[Exception](parse0(pAdt, "type T = A {} | A {}"))
  }

  test("exception: duplicate family names") {
    assertThrows[Exception](parse0(pFamDef(Prog), "Family A { Family C {} Family C {} }"))
  }

  test("can parse record fields that are constructors") {
    assert(canParse(pFieldName, "HelloWorld"))
  }

  test("can parse cases by themselves") {
    assert(canParse(pCasesDef, "cases hello_world <T> : {} -> {A: B -> N, C: B -> N} = " +
      "lam (_: {}). {A = lam (x: B). 3, C = lam (x: B). 4}"))
  }

  test("can parse extended definition syntax") {
    assert(canParse(pExtendedDef, """
      def plus(n1: N): Exp -> N =
        case EBase() = n1
        case ENat(n2:N) = n2
      """))
  }

  test("can parse extended definition syntax, empty context") {
    assert(canParse(pExtendedDef, """
      def ev: Exp -> N =
        case EBase() = 0
        case ENat(n:N) = n
      """))
  }

  test("can parse extended definition syntax, currying") {
    assert(canParse(pExp, "foo(a, b, c)(d)"))
  }

  test("can parse extended definition syntax, currying empty") {
    assert(canParse(pExp, "foo()(d)"))
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
    print(parse0(pProgram, fam))
    assert(canParse(pProgram, fam))
  }

  // The family definition that is parsed for STLCBase
  // (STLCBase,
  //   Linkage(
  //     AbsoluteFamily(Sp(Prog),STLCBase),
  //     SelfFamily(Sp(Prog),STLCBase),
  //     None,
  //     Map(),
  //     Map(),
  //     Map(
  //       Ty -> AdtDefn(Ty,Eq,
  //         DefnBody(
  //           Some(Map(
  //             TUnit -> RecType(Map()), 
  //             TNat -> RecType(Map()), 
  //             TArr -> RecType(Map(t1 -> PathType(None,Ty), t2 -> PathType(None,Ty))))),
  //           None,
  //           None, 
  //           List(Map(
  //             TUnit -> RecType(Map()), 
  //             TNat -> RecType(Map()), 
  //             TArr -> RecType(Map(t1 -> PathType(None,Ty), t2 -> PathType(None,Ty)))))
  //         )
  //       ), 
  //       Val -> AdtDefn(Val,Eq,
  //         DefnBody(
  //           Some(Map(
  //             Unit -> RecType(Map()), 
  //             Var -> RecType(Map(x -> NType)), 
  //             Lam -> RecType(Map(x -> NType, e -> PathType(None,Exp))))),
  //           None,
  //           None,
  //           List(Map(
  //             Unit -> RecType(Map()), 
  //             Var -> RecType(Map(x -> NType)), 
  //             Lam -> RecType(Map(x -> NType, e -> PathType(None,Exp)))))
  //         )
  //       ), 
  //       Exp -> AdtDefn(Exp,Eq,DefnBody(Some(Map(EVal -> RecType(Map(v -> PathType(None,Val))), EApp -> RecType(Map(e1 -> PathType(None,Exp), e2 -> PathType(None,Exp))))),None,None,List(Map(EVal -> RecType(Map(v -> PathType(None,Val))), EApp -> RecType(Map(e1 -> PathType(None,Exp), e2 -> PathType(None,Exp))))))), 
        
  //       OptionVal -> AdtDefn(OptionVal,Eq,DefnBody(Some(Map(SomeVal -> RecType(Map(v -> PathType(None,Val))), NoneVal -> RecType(Map()))),None,None,List(Map(SomeVal -> RecType(Map(v -> PathType(None,Val))), NoneVal -> RecType(Map())))))
  //     ),
        
  //     Map(
  //       eval -> FunDefn(eval,FunType(PathType(None,Exp),PathType(None,OptionVal)),
  //         DefnBody(Some(Lam(Var($m),PathType(None,Exp),
  //           Match(Var($m),FamCases(None,eval_cases),Rec(Map())))),
  //           None,
  //           None,
  //           List(Lam(Var($m),PathType(None,Exp),Match(Var($m),FamCases(None,eval_cases),Rec(Map()))))
  //         )
  //       ), 
  //       apply -> FunDefn(apply,FunType(PathType(None,Exp),FunType(PathType(None,Val),PathType(None,OptionVal))),
  //         DefnBody(Some(Lam(Var(_e2),PathType(None,Exp),Lam(Var($m),PathType(None,Val),Match(Var($m),FamCases(None,apply_cases),Rec(Map(e2 -> Var(_e2))))))),None,None,List(Lam(Var(_e2),PathType(None,Exp),Lam(Var($m),PathType(None,Val),Match(Var($m),FamCases(None,apply_cases),Rec(Map(e2 -> Var(_e2)))))))))),
        
  //     Map(
  //       eval_cases -> CasesDefn(eval_cases,PathType(None,Exp),FunType(RecType(Map()),RecType(Map(EVal -> FunType(RecType(Map(v -> PathType(None,Val))),PathType(None,OptionVal)), EApp -> FunType(RecType(Map(e1 -> PathType(None,Exp), e2 -> PathType(None,Exp))),PathType(None,OptionVal))))),List(FunType(RecType(Map()),RecType(Map(EVal -> FunType(RecType(Map(v -> PathType(None,Val))),PathType(None,OptionVal)), EApp -> FunType(RecType(Map(e1 -> PathType(None,Exp), e2 -> PathType(None,Exp))),PathType(None,OptionVal)))))),Eq,DefnBody(Some(Lam(Var($x),RecType(Map()),Rec(Map(EVal -> Lam(Var($m),RecType(Map(v -> PathType(None,Val))),Inst(PathType(None,SomeVal),Rec(Map(v -> Proj(Var($m),v))))), EApp -> Lam(Var($m),RecType(Map(e1 -> PathType(None,Exp), e2 -> PathType(None,Exp))),IfThenElse(App(Var(some),App(Var(eval),Proj(Var($m),e1))),App(Lam(Var(v),PathType(None,Val),App(Var(apply),App(Proj(Var($m),e2),Var(v)))),Proj(App(Var(eval),Proj(Var($m),e1)),v)),Inst(PathType(None,NoneVal),Rec(Map())))))))),None,None,List(Lam(Var($x),RecType(Map()),Rec(Map(EVal -> Lam(Var($m),RecType(Map(v -> PathType(None,Val))),Inst(PathType(None,SomeVal),Rec(Map(v -> Proj(Var($m),v))))), EApp -> Lam(Var($m),RecType(Map(e1 -> PathType(None,Exp), e2 -> PathType(None,Exp))),IfThenElse(App(Var(some),App(Var(eval),Proj(Var($m),e1))),App(Lam(Var(v),PathType(None,Val),App(Var(apply),App(Proj(Var($m),e2),Var(v)))),Proj(App(Var(eval),Proj(Var($m),e1)),v)),Inst(PathType(None,NoneVal),Rec(Map())))))))))), 
        
  //       apply_cases -> CasesDefn(apply_cases,PathType(None,Val),FunType(RecType(Map(e2 -> PathType(None,Exp))),RecType(Map(Lam -> FunType(RecType(Map(x -> NType, e -> PathType(None,Exp))),PathType(None,OptionVal)), _ -> FunType(RecType(Map()),PathType(None,OptionVal))))),List(FunType(RecType(Map(e2 -> PathType(None,Exp))),RecType(Map(Lam -> FunType(RecType(Map(x -> NType, e -> PathType(None,Exp))),PathType(None,OptionVal)), _ -> FunType(RecType(Map()),PathType(None,OptionVal)))))),Eq,DefnBody(Some(Lam(Var($x),RecType(Map(e2 -> PathType(None,Exp))),Rec(Map(Lam -> Lam(Var($m),RecType(Map(x -> NType, e -> PathType(None,Exp))),App(Var(eval),App(Var(subst),App(Proj(Var($m),x),App(Proj(Var($x),e2),Proj(Var($m),e)))))), _ -> Lam(Var($m),RecType(Map()),Inst(PathType(None,NoneVal),Rec(Map()))))))),None,None,List(Lam(Var($x),RecType(Map(e2 -> PathType(None,Exp))),Rec(Map(Lam -> Lam(Var($m),RecType(Map(x -> NType, e -> PathType(None,Exp))),App(Var(eval),App(Var(subst),App(Proj(Var($m),x),App(Proj(Var($x),e2),Proj(Var($m),e)))))), _ -> Lam(Var($m),RecType(Map()),Inst(PathType(None,NoneVal),Rec(Map())))))))))
  //     ),
      
  //     Map()))

}