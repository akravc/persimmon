import PersimmonSyntax.*
import TestParser._
import PrettyPrint._
import scala.io.Source
import PersimmonUtil.*

object PersimmonLinkages {

  /* ======================== Global vars ======================== */

  private var program: String = ""
  private var programTypLinkage: TypingLinkage = null
  private var programDefLinkage: DefinitionLinkage = null

  def p = program
  def p_=(aProgram: String) = {
    program = aProgram
    programTypLinkage = null
    programDefLinkage = null
  }
  /* ======================== Helpers ======================== */

  // class for exceptions
  case class LinkageException(s: String) extends Exception(s)

  // What kind of linkage are we computing?
  enum LinkageType: 
    case TypLink, DefLink

  /* ===================== Linkage Computation Rules ===================== */
  
  // L-Prog-Typ
  def computeLProgTyp(): TypingLinkage = {
    if (programTypLinkage != null) return programTypLinkage
    // if parsing successful
    if (canParse(pProgram, p)) {
      // return what was parsed
      programTypLinkage = parseProgramTypLink(p)
      programTypLinkage
    } else {
      throw new Exception("L-Prog-Def: Cannot parse the program.")
    }
  }

  // L-Prog-Def
  def computeLProgDef(): DefinitionLinkage = {
    if (programDefLinkage != null) return programDefLinkage
    // if parsing successful
    if (canParse(pProgram, p)) {
      // return what was parsed
      programDefLinkage = parseProgramDefLink(p)
      programDefLinkage
    } else {
      throw new Exception("L-Prog-Def: Cannot parse the program.")
    }
  }

  // L-Self
  def computeLSelf(a: Sp, opt: LinkageType): Linkage = {
    // can assume shape self(a.A) for path
    a.sp match {
      case SelfFamily(pref, fam) => 
        computeLNest(AbsoluteFamily(pref, fam), opt)
      case _ => throw new Exception("L-Self: Path shape is incorrect.")
    }
  }

  // L-Sub
  def computeLSub(a: AbsoluteFamily, opt: LinkageType): Linkage = {
    val lkg = computeLNest(a, opt)
    pathSub(lkg, a, Sp(SelfFamily(a.pref, a.fam)))
  }

  // L-Nest
  def computeLNest(a: AbsoluteFamily, opt: LinkageType): Linkage = {
    val lkgWrap = computeLinkage(a.pref, opt)
    val lkg = lkgWrap.getNestedLinkage(a.fam)
    
    lkg match {
      case Some(lkgA) => 
        val superPath = lkgA.getSuperPath()
        val superLkg = superPath match {
          case Some(p) => computeLNest(p, opt)
          case _ => opt match { 
            // no parent, so return dummy empty linkage
            case LinkageType.DefLink => 
              DefinitionLinkage(null, None, Map(), Map(), Map(), Map(), Map(), Map())
            case LinkageType.TypLink => 
              TypingLinkage(null, None, Map(), Map(), Map(), Map(), Map())
          }
        }
        concatenateLinkages(superLkg, lkgA)
      case _ => 
        throw new LinkageException("L-Nest: no nested linkage for family " + a.fam)
    }
  }

  // Universal computation function for linkages
  // opt 1 computes typing linkage, 
  // opt 2 computes definition linkage
  def computeLinkage(a: Path, opt: LinkageType): Linkage = {
    a match {
      // a.A ~> L (L-Sub applies)
      case AbsoluteFamily(pref, fam) => 
        computeLSub(a.asInstanceOf[AbsoluteFamily], opt)
      // L-Self or L-Prog applies
      case Sp(sp) => sp match {
        case Prog => 
          opt match {
            case LinkageType.DefLink => computeLProgDef()
            case LinkageType.TypLink => computeLProgTyp()
          }
        case SelfFamily(pref, fam) => 
          computeLSelf(a.asInstanceOf[Sp], opt)
      }
    }
  }
  
  // user friendly computation functions with built-in casting
  def computeTypLinkage(a: Path): TypingLinkage = {
    computeLinkage(a, LinkageType.TypLink).asInstanceOf[TypingLinkage]
  }

  def computeDefLinkage(a: Path): DefinitionLinkage = {
    computeLinkage(a, LinkageType.DefLink).asInstanceOf[DefinitionLinkage]
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
  def concatCasesSigs(cases1: Map[String, CasesSig], cases2: Map[String, CasesSig]): Map[String, CasesSig] = {
    val inheritedUnchanged = cases1.filter((name, sig1) => !cases2.contains(name))
    val newlyDefined = cases2.filter((name, sig2) => !cases1.contains(name))
    val difference = cases2.removedAll(newlyDefined.keys)

    val extended = difference.map{
      (name, sig2) => 
        // TODO: overriding case
        if (sig2.marker == Eq) then (name, sig2)
        // extension case
        else {
          val inheritedSig = cases1.get(name).get
          if (inheritedSig.matchType != sig2.matchType || 
            inheritedSig.t.input != sig2.t.input) then 
              throw LinkageException("Concatenating cases with incompatible types.")
            else {
              val combinedInpType = inheritedSig.t.input.asInstanceOf[RecordType]
              val combinedOutputType = concatRecordTypes(inheritedSig.t.output.asInstanceOf[RecordType], sig2.t.output.asInstanceOf[RecordType])

              (name, CasesSig(name, sig2.matchType, Eq, FunType(combinedInpType, combinedOutputType)))
            }
        }
    }

    inheritedUnchanged ++ extended ++ newlyDefined
  }

  def concatRecordTypes(r1: RecordType, r2: RecordType): RecordType = {
    if (r1.fields.forall((s, t) => !r2.fields.contains(s)) && 
      r2.fields.forall((s, t) => !r1.fields.contains(s))) 
    then RecordType(r1.fields ++ r2.fields) 
    else throw LinkageException("Concatenating cases with duplicate handler names.")
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
    val inheritedUnchanged = cases1.filter((name, def1) => !cases2.contains(name))
    val newlyDefined = cases2.filter((name, def2) => !cases1.contains(name))
    val difference = cases2.removedAll(newlyDefined.keys)

    val extended = difference.map{
      (name, def2) =>
        // TODO: overriding case
        if (def2.marker == Eq) then (name, def2)
        // extension case
        else {
          val inheritedDef = cases1.get(name).get
          if (inheritedDef.matchType != def2.matchType || 
            inheritedDef.t.input != def2.t.input) then 
              throw LinkageException("Concatenating cases with incompatible types.")
            else {
              val combinedInpType = inheritedDef.t.input.asInstanceOf[RecordType]
              val combinedOutputType = concatRecordTypes(inheritedDef.t.output.asInstanceOf[RecordType], def2.t.output.asInstanceOf[RecordType])
              val combinedBody = concatCasesBodies(inheritedDef.casesBody, def2.casesBody)

              (name, CasesDefn(name, def2.matchType, FunType(combinedInpType, combinedOutputType), Eq, combinedBody))
            }
        }
    }
    inheritedUnchanged ++ extended ++ newlyDefined
  }

  def concatCasesBodies(b1: Expression, b2: Expression): Expression = {
    val v = freshVar(boundVarsInExp(b1) ++ boundVarsInExp(b2))
    val body1 = b1 match {
      case Lam(v1, t, Record(body)) => 
        subVarInExp(Record(body), v1, v).asInstanceOf[Record].fields
      case _ => throw LinkageException("Body of cases definition is invalid.") // This should never happen.
    }
    val body2 = b2 match {
      case Lam(v2, t, Record(body)) => 
        subVarInExp(Record(body), v2, v).asInstanceOf[Record].fields
      case _ => throw LinkageException("Body of cases definition is invalid.") // This should never happen.
    }
    if (body1.forall((s, e) => !body2.contains(s)) && 
      body2.forall((s, e) => !body1.contains(s))) 
    then Lam(v, b1.asInstanceOf[Lam].t, Record(body1 ++ body2))
    else throw LinkageException("Concatenating cases with duplicate handler names.")
  }
}
