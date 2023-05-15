import PersimmonSyntax.*
import TestParser._
import PrettyPrint._
import scala.io.Source

object PersimmonLinkages {

  /* ======================== Global vars ======================== */

  // TODO: program, cache
  var p: String = ""

  /* ======================== Helpers ======================== */

  // class for exceptions
  case class LinkageException(s: String) extends Exception(s)

  // What kind of linkage are we computing?
  enum LinkageType: 
    case TypLink, DefLink

  /* ===================== Linkage Computation Rules ===================== */
  
  // L-Prog-Typ
  def computeLProgTyp(K: PathCtx): TypingLinkage = {
    // if parsing successful
    if (canParse(pProgram, p)) {
      // return what was parsed
      parseProgramTypLink(p)
    } else {
      throw new Exception("L-Prog-Def: Cannot parse the program.")
    }
  }

  // L-Prog-Def
  def computeLProgDef(K: PathCtx): DefinitionLinkage = {
    // if parsing successful
    if (canParse(pProgram, p)) {
      // return what was parsed
      parseProgramDefLink(p)
    } else {
      throw new Exception("L-Prog-Def: Cannot parse the program.")
    }
  }

  // L-Self
  def computeLSelf(K: PathCtx, a: Sp, opt: LinkageType): Linkage = {
    // can assume shape self(a.A) for path
    a.sp match {
      case SelfFamily(pref, fam) => 
        if (K.contains(SelfFamily(pref, fam))) {
          computeLNest(K, AbsoluteFamily(pref, fam), opt)
        } else throw new Exception("L-Self: Path is not in scope.")
      case _ => throw new Exception("L-Self: Path shape is incorrect.")
    }
  }

  // L-Sub
  def computeLSub(K: PathCtx, a: AbsoluteFamily, opt: LinkageType): Linkage = {
    val lkg = computeLNest(K, a, opt)
    pathSub(lkg, a, Sp(SelfFamily(a.pref, a.fam)))
  }

  // L-Nest
  def computeLNest(K: PathCtx, a: AbsoluteFamily, opt: LinkageType): Linkage = {
    val lkgWrap = computeLinkage(K, a.pref, opt)
    //printLkg(lkgWrap, "")
    val lkg = lkgWrap.getNestedLinkage(a.fam)
    
    lkg match {
      case Some(lkgA) => 
        val superPath = lkgA.getSuperPath()
        val superLkg = superPath match {
          case Some(p) => computeLNest(K, p, opt)
          case _ => opt match { 
            // no parent, so return dummy empty linkage
            case LinkageType.DefLink => 
              DefinitionLinkage(null, None, Map(), Map(), Map(), Map(), Map(), Map())
            case LinkageType.TypLink => 
              TypingLinkage(null, None, Map(), Map(), Map(), Map(), Map())
          }
        }
        // print("PRINTING SUPER")
        // printLkg(superLkg, "")
        concatenateLinkages(superLkg, lkgA)
      case _ => 
        throw new LinkageException("L-Nest: no nested linkage for family " + a.fam)
    }
  }

  // Universal computation function for linkages
  // opt 1 computes typing linkage, 
  // opt 2 computes definition linkage
  def computeLinkage(K: PathCtx, a: Path, opt: LinkageType): Linkage = {
    a match {
      // K |- a.A ~> L (L-Sub applies)
      case AbsoluteFamily(pref, fam) => 
        computeLSub(K, a.asInstanceOf[AbsoluteFamily], opt)
      // L-Self or L-Prog applies
      case Sp(sp) => sp match {
        case Prog => 
          opt match {
            case LinkageType.DefLink => computeLProgDef(K)
            case LinkageType.TypLink => computeLProgTyp(K)
          }
        case SelfFamily(pref, fam) => 
          computeLSelf(K, a.asInstanceOf[Sp], opt)
      }
    }
  }
  
  // user friendly computation functions with built-in casting
  def computeTypLinkage(K: PathCtx, a: Path): TypingLinkage = {
    computeLinkage(K, a, LinkageType.TypLink).asInstanceOf[TypingLinkage]
  }

  def computeDefLinkage(K: PathCtx, a: Path): DefinitionLinkage = {
    computeLinkage(K, a, LinkageType.DefLink).asInstanceOf[DefinitionLinkage]
  }

  /* ======================== Path Substitution ======================== */

  // Substitute path p2 in the linkage with path p1
  // lkg[p1/p2]
  // including prefix substitution
  def pathSub(lkg: Linkage, p1: Path, p2: Path): Linkage = {
    //print("substituting paths in linkage " + printLkg(lkg, "") + "\n")

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
    // print("substituting: "  + printPath(p2) + " with " + printPath(p1) + 
    // " in " + printPath(p) + " \n")
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



  /* ====================== LINKAGE CONCATENATION ====================== */

  // Rule CAT-TOP
  def concatenateLinkages(lkgSuper: Linkage, lkgExt: Linkage): Linkage = {
    // update paths in inherited code so that they refer to the extension
    var lkgP = pathSub(lkgSuper, lkgExt.getSelfPath(), lkgSuper.getSelfPath())

    (lkgP, lkgExt) match {
      // concat typing linkages
      case (TypingLinkage(sp1, sup1, types1, adts1, funs1, cases1, nested1), TypingLinkage(sp2, sup2, types2, adts2, funs2, cases2, nested2)) =>
        TypingLinkage(
          self = sp2,
          sup = sup2,
          types = concatTypes(types1, types2),
          adts = concatADTS(adts1, adts2),
          funs = concatFunSigs(funs1, funs2),
          cases = concatCasesSigs(cases1, cases2),
          nested = concatNestedLinkages(nested1, nested2).asInstanceOf[Map[String, TypingLinkage]]
        )
      // concat definition linkages
      case (DefinitionLinkage(sp1, sup1, types1, defaults1, adts1, funs1, cases1, nested1), DefinitionLinkage(sp2, sup2, types2, defaults2, adts2, funs2, cases2, nested2)) =>
        DefinitionLinkage(
          self = sp2,
          sup = sup2,
          types = concatTypes(types1, types2),
          defaults = concatDefaults(defaults1, defaults2),
          adts = concatADTS(adts1, adts2),
          funs = concatFunDefns(funs1, funs2),
          cases = concatCasesDefns(cases1, cases2),
          nested = concatNestedLinkages(nested1, nested2).asInstanceOf[Map[String, DefinitionLinkage]]
        )
      case (_, _) => throw LinkageException("Concatenating incompatible LinkageTypes.")
    }
  }

  // Rule CAT-NEST
  def concatNestedLinkages(nest1: Map[String, Linkage], nest2: Map[String, Linkage]): Map[String, Linkage] = {
    nest1.flatMap { (name, linkage1) =>
      if nest2.contains(name) then None else Some((name, linkage1))
    } ++ nest2.map { (name, linkage2) =>
      nest1.get(name) match {
        case Some(linkage1) => (name, concatenateLinkages(linkage1, linkage2))
        case None => (name, linkage2)
      }
    }
  }

  // Rule CAT-TYPES
  def concatTypes(types1: Map[String, TypeDefn], types2:Map[String, TypeDefn]): Map[String, TypeDefn] = {
    types1.flatMap { (name, type1) =>
      if types2.contains(name) then None else Some((name, type1))
    } ++ types2.map { (name, type2) =>
      types1.get(name) match {
        case Some(type1) =>
          if type2.marker == Eq then
            throw LinkageException("Concattenating linkages with duplicate type definitions.")
          else if type2.typeBody.fields.keySet.exists { name =>
            type1.typeBody.fields.contains(name)
          } then throw LinkageException("Concattenating types with duplicate fields.")
          else (name, TypeDefn(name, Eq, RecordType(type1.typeBody.fields ++ type2.typeBody.fields)))
        case None => (name, type2)
      }
    }
  }

  // Rule CAT-ADTS
  def concatADTS(adts1: Map[String, AdtDefn], adts2: Map[String, AdtDefn]): Map[String, AdtDefn] = {
    adts1.flatMap { (name, adt1) =>
      if adts2.contains(name) then None else Some((name, adt1))
    } ++ adts2.map { (name, adt2) =>
      adts1.get(name) match {
        case Some(adt1) =>
          if adt2.marker == Eq then
            throw LinkageException("Concattenating linkages with duplicate ADT definitions.")
          else if adt2.adtBody.keySet.exists { name =>
            adt1.adtBody.contains(name)
          } then throw LinkageException("Concattenating types with duplicate constructors.") 
          else (name, AdtDefn(name, Eq, adt1.adtBody ++ adt2.adtBody))
        case None => (name, adt2)
      }
    }
  }

  // Rule CAT-DEFAULTS
  def concatDefaults(defs1: Map[String, DefaultDefn], defs2: Map[String, DefaultDefn]): Map[String, DefaultDefn] = {
    defs1.flatMap { (name, def1) =>
      if defs2.contains(name) then None else Some((name, def1))
    } ++ defs2.map { (name, def2) =>
      defs1.get(name) match {
        case Some(def1) =>
          if def2.marker == Eq then
            throw LinkageException("Concattenating linkages with duplicate type definitions.") 
          else if def2.defaultBody.fields.keySet.exists { name =>
            def1.defaultBody.fields.contains(name)
          } then throw LinkageException("Concattenating types with duplicate default values.") 
          else (name, DefaultDefn(name, Eq, Record(def1.defaultBody.fields ++ def2.defaultBody.fields)))
        case None => (name, def2)
      }
    }
  }

  // Rule CAT-FUNS-TYP
  def concatFunSigs(funs1: Map[String, FunSig], funs2: Map[String, FunSig]): Map[String, FunSig] = {
    funs1.flatMap { (name, fun1) =>
      if funs2.contains(name) then None else Some((name, fun1))
    } ++ funs2.map { (name, fun2) =>
      funs1.get(name) match {
        case Some(fun1) =>
          if fun1.t != fun2.t then
            throw LinkageException("Concattenating linkages with duplicate incompatible function signatures.")
          else (name, fun1)
        case None => (name, fun2)
      }
    }
  }
  
  // Rule CAT-CASES-TYP
  // This should definitely be checked.
  def concatCasesSigs(cases1: Map[String, CasesSig], cases2: Map[String, CasesSig]): Map[String, CasesSig] = {
    cases1.flatMap { (name, case1) => // Calling the variable "case" for consistency with other functions, even though it represents a list of cases
      if cases2.contains(name) then None else Some((name, case1))
    } ++ cases2.map { (name, case2) =>
      cases1.get(name) match {
        case Some(case1) =>
          // TODO: I'm not 100% sure why having "=" here is useful.
          if case2.marker == Eq then
            if case1.matchType != case2.matchType || case1.t != case2.t then
              throw LinkageException("Concattenating linkages with duplicate incompatible cases signatures.")
            else (name, case1)
          else {
            val rec1 = case1.t.output match {
              case RecordType(rec) => rec
              case _ => throw LinkageException("Output type for cases signature is not a record type.") // This should never happen.
            };
            val rec2 = case2.t.output match {
              case RecordType(rec) => rec
              case _ => throw LinkageException("Output type for cases signature is not a record type.") // This should never happen.
            };
            val rec: Map[String, Type] = rec1.flatMap { (fieldName, t1) =>
              if rec2.contains(fieldName) then None else Some((name, t1))
            } ++ rec2.map { (fieldName, t2) =>
              rec1.get(name) match {
                case Some(t1) =>
                  if t1 != t2 then
                    throw LinkageException("Concattenating linkages with incompatible cases signatures.")
                  else (fieldName, t1)
                case None => (fieldName, t2)
              }
            };
            if case1.matchType != case2.matchType || case1.t.input != case2.t.input then
              throw LinkageException("Concattenating linkages with incompatible cases signatures.")
            else (name, CasesSig(name, case1.matchType, Eq, FunType(case1.t.input, RecordType(rec))))
          }
        case None => (name, case2)
      }
    }
  }

  // Rule CAT-FUNS-DEF
  def concatFunDefns(funs1: Map[String, FunDefn], funs2: Map[String, FunDefn]): Map[String, FunDefn] = {
    funs1.flatMap { (name, fun1) =>
      if funs2.contains(name) then None else Some((name, fun1))
    } ++ funs2.map { (name, fun2) =>
      funs1.get(name) match {
        case Some(fun1) =>
          if fun1.t != fun2.t then
            throw LinkageException("Concattenating linkages with incompatible function signatures.")
          else (name, FunDefn(fun1.name, fun1.t, fun2.funBody))
        case None => (name, fun2)
      }
    }
  }
  


  // Rule CAT-CASES-DEF
  def concatCasesDefns(cases1: Map[String, CasesDefn], cases2: Map[String, CasesDefn]): Map[String, CasesDefn] = {
    cases1.flatMap { (name, case1) =>
      if cases2.contains(name) then None else Some((name, case1))
    } ++ cases2.map { (name, case2) =>
      cases1.get(name) match {
        case Some(case1) =>
          if case2.marker == Eq then
            if case1.matchType != case2.matchType || case1.t != case2.t then
              throw LinkageException("Concattenating linkages with duplicate incompatible cases signatures.")
            else (name, case2)
          else {
            // TODO: Currently redoing all the computation done for cases signatures. Not sure if this is intended.
            val rec1 = case1.t.output match {
              case RecordType(rec) => rec
              case _ => throw LinkageException("Output type for cases signature is not a record type.") // This should never happen.
            };
            val rec2 = case2.t.output match {
              case RecordType(rec) => rec
              case _ => throw LinkageException("Output type for cases signature is not a record type.") // This should never happen.
            };
            val rec: Map[String, Type] = rec1.flatMap { (fieldName, t1) =>
              if rec2.contains(fieldName) then None else Some((name, t1))
            } ++ rec2.map { (fieldName, t2) =>
              rec1.get(name) match {
                case Some(t1) =>
                  if t1 != t2 then
                    throw LinkageException("Concattenating linkages with incompatible cases signatures.")
                  else (fieldName, t1)
                case None => (fieldName, t2)
              }
            };
            if case1.matchType != case2.matchType || case1.t.input != case2.t.input then
              throw LinkageException("Concattenating linkages with incompatible cases signatures.")
            // generate var that is fresh to both cases bodies
            val v = freshVar(boundVarsInExp(case1.casesBody) ++ 
                      boundVarsInExp(case2.casesBody))
            val body1 = case1.casesBody match {
              case Lam(v1, t, Record(body)) => 
                subVarInExp(Record(body), v1, v).asInstanceOf[Record].fields
              case _ => throw LinkageException("Body of cases definition is invalid.") // This should never happen.
            }
            val body2 = case2.casesBody match {
              case Lam(v2, t, Record(body)) => 
                subVarInExp(Record(body), v2, v).asInstanceOf[Record].fields
              case _ => throw LinkageException("Body of cases definition is invalid.") // This should never happen.
            }
            val body: Map[String, Expression] = body1.flatMap { (fieldName, e1) =>
              if body2.contains(fieldName) then None else Some((fieldName, e1))
            } ++ body2;
            (name, CasesDefn(name, case1.matchType, FunType(case1.t.input, RecordType(rec)), Eq, Lam(v, case1.t.input, Record(body))))
          }
        case None => (name, case2)
      }
    }
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
    var alphabet = ('a' to 'z') ++ ('A' to 'Z')
    var x = "" + alphabet(scala.util.Random.nextInt(52))
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
}