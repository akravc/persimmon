import PersimmonSyntax.*
import PersimmonTyping.*
import PersimmonLinkages.*
import PrettyPrint.*
import scala.io.Source

// Utility functions needed throughout for pre-processing linkages, 
// unfolding wildcard cases, etc.

object PersimmonUtil {

/*================ UNFOLD WILDCARD CASES ================*/

def unfoldWildcards(lkg: DefinitionLinkage): DefinitionLinkage = {
  DefinitionLinkage(lkg.self, lkg.sup, lkg.types, lkg.defaults, lkg.adts, 
    lkg.funs, lkg.cases.map{(s, cd) => (s, unfoldWildcardsInCasesDefn(lkg, cd))}, lkg.nested.map{(s, nd) => (s, unfoldWildcards(nd))})
}

def unfoldWildcardsInCasesDefn(lkg: DefinitionLinkage, cd: CasesDefn): CasesDefn = {
  val casesOutType = cd.t.output.asInstanceOf[RecordType]
  // if no wildcards to unfold, return as is
  if !casesOutType.fields.contains("_") then cd else {
    val adtDef = lkg.adts.get(cd.matchType.name)
    // if no such ADT, simply return as it is not WF
    if adtDef == None then cd else {
      // constructing the unfolded output type
      val typeName = adtDef.get.name
      val adtBody = adtDef.get.adtBody
      val nonWildcardHandlerTypes = 
        casesOutType.fields.filter((s, t) => (s != "_"))
      val matchOutType = 
        casesOutType.fields.head._2.asInstanceOf[FunType].output
      val adtCtorsCoveredByWildcard = adtBody.--(nonWildcardHandlerTypes.keys)
      val wildcardCoveredHandlerTypes = 
        adtCtorsCoveredByWildcard.map((s, rt) => (s, FunType(rt, matchOutType)))
      val casesUnfoldedOutType = 
        RecordType(nonWildcardHandlerTypes ++ wildcardCoveredHandlerTypes)

      // constructing the unfolded cases body
      val casesBody = cd.casesBody.asInstanceOf[Lam]
      val recordOfHandlers = 
        casesBody.body.asInstanceOf[Record]
      val wildcardHandler = 
        recordOfHandlers.fields.get("_").get.asInstanceOf[Lam]
      val nonWildcardHandlers = 
        recordOfHandlers.fields.filter((s, e) => (s != "_"))
      val wildcardCoveredHandlers = 
        adtCtorsCoveredByWildcard.map((s, rt) => (s, Lam(Var("_"), rt, wildcardHandler.body)))
      val casesUnfoldedBody = 
        Lam(casesBody.v, casesBody.t,
          Record(nonWildcardHandlers ++ wildcardCoveredHandlers))

      // constructing the casesDefn
      CasesDefn(cd.name, cd.matchType, 
        FunType(cd.t.input, casesUnfoldedOutType), cd.ts, cd.marker, casesUnfoldedBody)
    }
  }
}

/* ======================== Path Substitution ======================== */

  // Substitute path p2 in the linkage with path p1
  // lkg[p1/p2]
  // including prefix substitution
  def pathSub(lkg: Linkage, p1: Path, p2: Path): Linkage = {

    if (lkg == null) {
      throw new LinkageException("Cannot substitute paths in a null linkage.")
    }
    
    // new self
    val newself = subPathInPath(lkg.getSelfPath(), p1, p2)

    // new super
    val newsup = lkg.getSuperPath() match {
      case None => None
      case Some(p) => Some(subPathInPath(p, p1, p2).asInstanceOf[AbsoluteFamily])
    }

    val newtypes = lkg.getTypes().map{ (s, td) => (s, subPathInTypeDefn(td, p1, p2))}
    val newadts = lkg.getAdts().map{ (s, adt) => (s, subPathInAdt(adt, p1, p2))}

    lkg match {
      case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => 
        val newdefaults = defaults.map{ (s, dd) => (s, subPathInDefaultDefn(dd, p1, p2))}
        val newfuns = funs.map{ (s, fdef) => (s, subPathInFunDefn(fdef, p1, p2))}
        val newcases = cases.map{ (s, cdef) => (s, subPathInCasesDefn(cdef, p1, p2))}
        val newnested = nested.map( (s, link) => (s, pathSub(link, p1, p2).asInstanceOf[DefinitionLinkage]))
        DefinitionLinkage(newself, newsup, newtypes, newdefaults, newadts, newfuns, newcases, newnested)
      case TypingLinkage(self, sup, types, adts, funs, cases, nested) => 
        val newfuns = funs.map{ (s, fsig) => (s, subPathInFunSig(fsig, p1, p2))}
        val newcases = cases.map{ (s, csig) => (s, subPathInCasesSig(csig, p1, p2))}
        val newnested = nested.map( (s, link) => (s, pathSub(link, p1, p2).asInstanceOf[TypingLinkage]))
        TypingLinkage(newself, newsup, newtypes, newadts, newfuns, newcases, newnested)
    }
  }

  // substitute path p2 in any path p with p1
  // including any prefix of p if it matches
  def subPathInPath(p: Path, p1: Path, p2: Path): Path = {
    if (p == p2) then p1 else
    p match {
      case Sp(sp) => 
        sp match {
          case Prog => 
            p // don't sub prog, nothing extends prog.
          case SelfFamily(pref, fam) => 
            Sp(SelfFamily(subPathInPath(pref, p1, p2), fam))
        }
      case AbsoluteFamily(pref, fam) => 
        AbsoluteFamily(subPathInPath(pref, p1, p2), fam)
    }
  }

  def subPathInType(t: Type, p1: Path, p2: Path): Type = {
    t match {
      case FunType(input, output) => 
        FunType(subPathInType(input, p1, p2), subPathInType(output, p1, p2))
      case PathType(path, name) => 
        path match {
          case None => t
          case Some(p) => PathType(Some(subPathInPath(p, p1, p2)), name)
        }
      case RecordType(fields) => 
        RecordType(fields.map( (s, t) => (s, subPathInType(t, p1, p2))))
      case _ => t
    }
  }

  def subPathInExp(e: Expression, p1: Path, p2: Path): Expression = {
    e match {
      case App(e1, e2) => App(subPathInExp(e1, p1, p2), subPathInExp(e2, p1, p2))
      case Plus(e1, e2) => Plus(subPathInExp(e1, p1, p2), subPathInExp(e2, p1, p2))
      case FamCases(path, name) => 
        if (path == Some(p2)) then FamCases(Some(p1), name) else e
      case FamFun(path, name) => 
        if (path == Some(p2)) then FamFun(Some(p1), name) else e
      case IfThenElse(condExpr, ifExpr, elseExpr) => 
        IfThenElse(subPathInExp(condExpr, p1, p2), subPathInExp(ifExpr, p1, p2), subPathInExp(elseExpr, p1, p2))
      case Inst(t, rec) => 
        Inst(subPathInType(t, p1, p2).asInstanceOf[PathType], subPathInExp(rec, p1, p2).asInstanceOf[Record])
      case InstADT(t, cname, rec) => 
        InstADT(subPathInType(t, p1, p2).asInstanceOf[PathType], cname, subPathInExp(rec, p1, p2).asInstanceOf[Record])
      case Lam(v, t, body) => 
        Lam(v, subPathInType(t, p1, p2), subPathInExp(body, p1, p2))
      case Match(e, c, r) => 
        Match(subPathInExp(e, p1, p2), subPathInExp(c, p1, p2).asInstanceOf[FamCases], subPathInExp(r, p1, p2).asInstanceOf[Record])
      case Proj(e, name) => Proj(subPathInExp(e, p1, p2), name)
      case Record(fields) => 
        Record(fields.map((s, r) => (s, subPathInExp(r, p1, p2))))
      case _ => e
    }
  }

  def subPathInTypeDefn(td: TypeDefn, p1: Path, p2: Path): TypeDefn = {
    TypeDefn(
      td.name, td.marker, 
      subPathInType(td.typeBody, p1, p2).asInstanceOf[RecordType])
  }

  def subPathInDefaultDefn(dd: DefaultDefn, p1: Path, p2: Path): DefaultDefn = {
    DefaultDefn(
      dd.name, dd.marker, 
      subPathInExp(dd.defaultBody, p1, p2).asInstanceOf[Record])
  }

  def subPathInAdt(adt: AdtDefn, p1: Path, p2: Path): AdtDefn = {
    AdtDefn(
      adt.name, adt.marker, 
      adt.adtBody.map((s, rt) => (s, subPathInType(rt, p1, p2).asInstanceOf[RecordType])))
  }

  def subPathInFunDefn(fd: FunDefn, p1: Path, p2: Path): FunDefn = {
    FunDefn(
      fd.name, subPathInType(fd.t, p1, p2).asInstanceOf[FunType], 
      subPathInExp(fd.funBody, p1, p2).asInstanceOf[Lam])
  }

  def subPathInCasesDefn(cd: CasesDefn, p1: Path, p2: Path): CasesDefn = {
    CasesDefn(cd.name, subPathInType(cd.matchType, p1, p2).asInstanceOf[PathType], subPathInType(cd.t, p1, p2).asInstanceOf[FunType], 
    cd.ts.map((t) => (subPathInType(t, p1, p2))), cd.marker,
    subPathInExp(cd.casesBody, p1, p2))
  }

  def subPathInFunSig(fs: FunSig, p1: Path, p2: Path): FunSig = {
    FunSig(
      fs.name, subPathInType(fs.t, p1, p2).asInstanceOf[FunType])
  }

  def subPathInCasesSig(cs: CasesSig, p1: Path, p2: Path): CasesSig = {
    CasesSig(cs.name, subPathInType(cs.matchType, p1, p2).asInstanceOf[PathType], 
    cs.marker, subPathInType(cs.t, p1, p2).asInstanceOf[FunType])
  }

/* ====================== READ FILE ====================== */

def readFile(filename: String): String = { 
  return Source.fromFile(filename).getLines.mkString("\n")
}

/* ====================== FRESH VARIABLES ====================== */

  // returns all variables bound in this expression
  def boundVarsInExp(e: Expression): List[String] = {
    e match {
      case App(e1, e2) => boundVarsInExp(e1) ++ boundVarsInExp(e2)
      case Plus(e1, e2) => boundVarsInExp(e1) ++ boundVarsInExp(e2)
      case IfThenElse(condExpr, ifExpr, elseExpr) => 
        boundVarsInExp(condExpr) ++ boundVarsInExp(ifExpr) ++ boundVarsInExp(elseExpr)
      case Inst(t, rec) => boundVarsInExp(rec)
      case InstADT(t, cname, rec) => boundVarsInExp(rec)
      case Lam(v, t, body) => List(v.id) ++ boundVarsInExp(body)
      case Match(e, c, r) => boundVarsInExp(e) ++ boundVarsInExp(r)
      case Proj(e, name) => boundVarsInExp(e)
      case Record(fields) => 
        fields.map((s, e) => (s, boundVarsInExp(e))).values.foldLeft(List())( (a: List[String], b: List[String]) => a ++ b)
      case _ => List()
    }
  }

  def freshVar(bound: List[String]): Var = {
    val alphabet = ('a' to 'z') ++ ('A' to 'Z')
    val x = "" + alphabet(scala.util.Random.nextInt(52))
    if (bound.contains(x)) then freshVar(bound)
    else Var(x)
  }

  /* ====================== VARIABLE SUBSTITUTION ====================== */

  // Substitute variable v2 with variable v1 in expression e
  // e [v1 / v2]
  def subVarInExp(e: Expression, v1: Var, v2: Var): Expression = {
    e match {
      case Var(id) => if e == v2 then v1 else e
      case App(e1, e2) => App(subVarInExp(e1, v1, v2), subVarInExp(e2, v1, v2))
      case Plus(e1, e2) => Plus(subVarInExp(e1, v1, v2), subVarInExp(e2, v1, v2))
      case IfThenElse(condExpr, ifExpr, elseExpr) => 
        IfThenElse(subVarInExp(condExpr, v1, v2), subVarInExp(ifExpr, v1, v2), subVarInExp(elseExpr, v1, v2))
      case Inst(t, rec) => 
        Inst(t, subVarInExp(rec, v1, v2).asInstanceOf[Record])
      case InstADT(t, cname, rec) => 
        InstADT(t, cname, subVarInExp(rec, v1, v2).asInstanceOf[Record])
      case Lam(v, t, body) => 
        if (v2 == v) 
        then Lam(v1, t, subVarInExp(body, v1, v2))
        else Lam(v, t, subVarInExp(body, v1, v2))
      case Match(e, c, r) => 
        Match(subVarInExp(e, v1, v2), c, subVarInExp(r, v1, v2).asInstanceOf[Record])
      case Proj(e, name) => Proj(subVarInExp(e, v1, v2), name)
      case Record(fields) => 
        Record(fields.map((s, r) => (s, subVarInExp(r, v1, v2))))
      case _ => e
    }
  }

/*================ RESOLVE FUN CALLS PARSED AS VARIABLES ================*/

  def resolveFunCalls(lkg: DefinitionLinkage): DefinitionLinkage = {
    val selfpath = lkg.self
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
    val selfpath = lkg.self
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
