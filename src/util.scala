import PersimmonSyntax.*
import PersimmonTyping.*
import PersimmonLinkages.*
import PrettyPrint.*

// Utility functions needed throughout for pre-processing linkages, 
// unfolding wildcard cases, etc.

object PersimmonUtil {

/*================ UNFOLD WILDCARD CASES ================*/

// def getCasesOutType(t: FunType): RecordType = {
//     while (t.output.isInstanceOf[FunType]) {
//         getCasesOutType(t.output.asInstanceOf[FunType])
//     }
//     if (!t.output.isInstanceOf[RecordType]) {
//         throw new Exception("Cases output type is not a record type. \n")
//     }
//     t.output.asInstanceOf[RecordType]
// }

// def getCasesInpType(t: FunType): RecordType = {
//     while (!t.output.isInstanceOf[RecordType]) {
//         getCasesOutType(t.output.asInstanceOf[FunType])
//     }
//     if (!t.input.isInstanceOf[RecordType]) {
//         throw new Exception("Cases input type is not a record type. \n")
//     }
//     t.input.asInstanceOf[RecordType]
// }


/*================ RESOLVE FUN CALLS PARSED AS VARIABLES ================*/

  def resolveFunCalls(lkg: DefinitionLinkage): DefinitionLinkage = {
    var selfpath = lkg.self
    DefinitionLinkage(
      lkg.self, lkg.sup, 
      lkg.types,
      lkg.defaults.map((s, ddef) => 
        (s, DefaultDefn(ddef.name, ddef.marker, resolveFunCallsInExp(ddef.defaultBody, selfpath, List()).asInstanceOf[Record]))),
      lkg.adts,
      lkg.funs.map((s, fd) => 
        (s, FunDefn(fd.name, fd.t, resolveFunCallsInExp(fd.funBody, selfpath, List()).asInstanceOf[Lam]))),
      lkg.cases.map((s, cd) => (s, CasesDefn(cd.name, cd.matchType, cd.t, 
        cd.ts, cd.marker, resolveFunCallsInExp(cd.casesBody, selfpath, List())))),
      lkg.nested.map((s, l) => (s, resolveFunCalls(l)))
    )
  }

  def resolveFunCallsInExp(e: Expression, sp: Path, bound: List[Var]): Expression = {
    e match {
      case Var(id) => if !bound.contains(e) then FamFun(Some(sp), id) else e
      case Lam(v, t, body) => 
        Lam(v, t, resolveFunCallsInExp(body, sp, (List(v) ++ bound)))
      case App(e1, e2) => 
        App(resolveFunCallsInExp(e1, sp, bound), resolveFunCallsInExp(e2, sp, bound))
      case Plus(e1, e2) => Plus(resolveFunCallsInExp(e1, sp, bound), resolveFunCallsInExp(e2, sp, bound))
      case Record(fields) => 
        Record(fields.map( (s, ex) => (s, resolveFunCallsInExp(ex, sp, bound))))
      case Proj(e, name) => Proj(resolveFunCallsInExp(e, sp, bound), name)
      case Inst(t, rec) => 
        Inst(t, resolveFunCallsInExp(rec, sp, bound).asInstanceOf[Record])
      case InstADT(t, cname, rec) => 
        InstADT(t, cname, resolveFunCallsInExp(rec, sp, bound).asInstanceOf[Record])
      case Match(e, c, r) => 
        Match(resolveFunCallsInExp(e, sp, bound), c, resolveFunCallsInExp(r, sp, bound).asInstanceOf[Record])
      case IfThenElse(condExpr, ifExpr, elseExpr) => 
        IfThenElse(resolveFunCallsInExp(condExpr, sp, bound), resolveFunCallsInExp(ifExpr, sp, bound), resolveFunCallsInExp(elseExpr, sp, bound))
      case _ => e
    }
  }

  /*================ FILL IMPLIED SELF-PREFIXES IN TYPES ================*/

  def fillNonePaths(lkg: DefinitionLinkage): DefinitionLinkage = {
    var selfpath = lkg.self
    DefinitionLinkage(
      lkg.self, lkg.sup, 
      lkg.types.map((s, tdef) => 
        (s, TypeDefn(tdef.name, tdef.marker, fillNonePathsInType(tdef.typeBody, selfpath).asInstanceOf[RecordType]))),
      lkg.defaults.map((s, ddef) => 
        (s, DefaultDefn(ddef.name, ddef.marker, fillNonePathsInExp(ddef.defaultBody, selfpath).asInstanceOf[Record]))),
      lkg.adts.map((s, adef) => 
        (s, AdtDefn(adef.name, adef.marker, 
          adef.adtBody.map((c, rt) => 
            (c, fillNonePathsInType(rt, selfpath).asInstanceOf[RecordType]))))),
      lkg.funs.map((s, fd) => 
        (s, FunDefn(fd.name, fillNonePathsInType(fd.t, selfpath).asInstanceOf[FunType], fillNonePathsInExp(fd.funBody, selfpath).asInstanceOf[Lam]))),
      lkg.cases.map((s, cd) => (s, CasesDefn(cd.name, 
      fillNonePathsInType(cd.matchType, selfpath).asInstanceOf[PathType], 
      fillNonePathsInType(cd.t, selfpath).asInstanceOf[FunType], 
      cd.ts.map(x => fillNonePathsInType(x, selfpath)), cd.marker, 
      fillNonePathsInExp(cd.casesBody, selfpath)))),
      lkg.nested.map((s, l) => (s, fillNonePaths(l)))
    )
  }

  def fillNonePathsInType(t: Type, p: Path): Type = {
    t match {
      case FunType(input, output) => 
        FunType(fillNonePathsInType(input, p), fillNonePathsInType(output, p))
      case PathType(path, name) => 
        if (path == None) then PathType(Some(p), name) else t
      case RecordType(fields) => 
        RecordType(fields.map{(s, t) => (s, fillNonePathsInType(t, p))})
      case _ => t
    }
  }

  def fillNonePathsInExp(e: Expression, p: Path): Expression = {
    e match {
      case Lam(v, t, body) => 
        Lam(v, fillNonePathsInType(t, p), fillNonePathsInExp(body, p))
      case FamFun(path, name) =>
        if (path == None) then FamFun(Some(p), name) else e
      case FamCases(path, name) =>
        if (path == None) then FamCases(Some(p), name) else e
      case App(e1, e2) => App(fillNonePathsInExp(e1, p), fillNonePathsInExp(e2, p))
      case Plus(e1, e2) => Plus(fillNonePathsInExp(e1, p), fillNonePathsInExp(e2, p))
      case Record(fields) => 
        Record(fields.map{(s, d) => (s, fillNonePathsInExp(d, p))})
      case Proj(r, name) => Proj(fillNonePathsInExp(r, p), name)
      case Inst(t, rec) => Inst(fillNonePathsInType(t, p).asInstanceOf[PathType], fillNonePathsInExp(rec, p).asInstanceOf[Record])
      case InstADT(t, cname, rec) => InstADT(fillNonePathsInType(t, p).asInstanceOf[PathType], cname, fillNonePathsInExp(rec, p).asInstanceOf[Record])
      case Match(m, c, r) => Match(fillNonePathsInExp(m, p), fillNonePathsInExp(c, p).asInstanceOf[FamCases], fillNonePathsInExp(r, p).asInstanceOf[Record])
      case IfThenElse(condExpr, ifExpr, elseExpr) => 
        IfThenElse(fillNonePathsInExp(condExpr, p), fillNonePathsInExp(ifExpr, p), fillNonePathsInExp(elseExpr, p))
      case _ => e
    }
  }
}