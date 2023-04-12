import PersimmonSyntax.*
import PersimmonTyping.*
import PersimmonLinkages.*

object PersimmonWF {

  // Well-formedness of definitions
  // the top level rule recursively checks 
  // the definition linkage for path prog
  def wfDef(K: PathCtx, lkg: DefinitionLinkage): Boolean = {
    // Notes:
    // - K here corresponds to sp::K in WF-FamDef the paper.
    // - lkg.self corresponds to self(sp.A) in WF-FamDef in the paper.
    // TODO: Is this correct? The way rule EC-Nest is laid out makes me unsure.
    if ancestors(lkg.self).contains(lkg.self) then false
    else {
      val K_prime = List(lkg.self) ++ K
      val L_S = computeTypLinkage(K_prime, Sp(lkg.self))
      exhaustivityCheck(K_prime, L_S) &&
      lkg.nested.forall { (_, nested_lkg) => wfDef(K_prime, nested_lkg) } &&
      lkg.types.forall { (name, td) =>
        if td.marker == Eq then wfTypDef(K_prime, td)
        else lkg.defaults.contains(name) && wfTypDefExt(K_prime, td, lkg.defaults(name))
      } &&
      lkg.adts.forall { (_, adt) => wfAdtDef(K_prime, adt) } &&
      lkg.funs.forall { (_, fd) => wfFunDef(K_prime, fd) } &&
      lkg.cases.forall { (_, cd) => wfCasesDef(K_prime, cd) }
    }
  }

  // this recursively gets all paths from the program 
  // by traversing the typing linkage for prog
  def allPathsContext(): List[SelfPath] = {
    var lkg = computeTypLinkage(List(), Sp(Prog))
    collectAllPathsWithin(lkg)
  }

  def collectAllPathsWithin(lkg: TypingLinkage): List[SelfPath] = {
    var lstResult = lkg.self :: List()
    for ((famName, nestLkg) <- lkg.nested) {
      lstResult = collectAllPathsWithin(nestLkg) ++ lstResult
    }
    lstResult
  }

  // ancestors function
  // TODO: make sure the context passed into there has ALL paths
  // in the program
  def ancestors(p: SelfPath): List[SelfPath] = {
    var K = allPathsContext()
    var currLkg = computeTypLinkage(K, Sp(p))
    currLkg.getSuperPath() match {
      case Some(p) => relativizePath(p) :: ancestors(relativizePath(p))
      case None => return List()
    } 
  }

  // well-formedness of type definitions
  def wfTypDef(K: PathCtx, td: TypeDefn): Boolean = {
    wfType(K, td.typeBody);
  }

  // well-formedness of type extensions and their defaults
  def wfTypDefExt(K: PathCtx, td: TypeDefn, dd: DefaultDefn): Boolean = {
    wfType(K, td.typeBody) && dd.defaultBody.fields.forall { (name, e) =>
      td.typeBody.fields.contains(name) &&
      hasType(K, Map(), e, td.typeBody.fields(name))
    }
  }

  // well-formedness of ADT definitions
  def wfAdtDef(K: PathCtx, adt: AdtDefn): Boolean = {
    adt.adtBody.forall { (name, rec) => wfType(K, rec) }
  }

  // well-formedness of functions
  def wfFunDef(K: PathCtx, fd: FunDefn): Boolean = {
    wfType(K, fd.t) && hasType(K, Map(), fd.funBody, fd.t)
  }

  // well-formedness of cases definitions
  def wfCasesDef(K: PathCtx, cd: CasesDefn): Boolean = {
    val L_S = computeTypLinkage(K, cd.matchType.path.get);
    L_S.adts.contains(cd.matchType.name) &&
    (cd.t.output match {
      // TODO: I think the way this is written in the rule WF-CasesDef is confusing because 'C' and 'T' are used for both 'R' and the case names/types in the cases definition.
      case RecType(rec) => rec.forall { (name, t) =>
        L_S.adts(cd.matchType.name).adtBody.get(name) == Some(t)
      }
      case _ => throw Exception("Output type for cases signature is not a record type.") // TODO: Is this the right way to handle this error?
    }) &&
    wfType(K, cd.t) &&
    hasType(K, Map(), cd.casesBody, cd.t)
  }

  // rule EC-Nest
  def exhaustivityCheck(K: PathCtx, lkg: TypingLinkage): Boolean = {
    lkg.cases.forall { (name, cases) => {
      val L_S_prime = computeTypLinkage(K, cases.mt.path.get)
      L_S_prime.adts.contains(name) &&
      (cases.t.output match {
        case RecType(rec) => L_S_prime.adts(name).adtBody == rec // TODO: Does this do what I want it to do?
        case _ => throw Exception("Output type for cases signature is not a record type.") // TODO: Is this the right way to handle this error?
      })
    }} &&
    lkg.nested.forall { (name, A) => {
      // TODO: Is this equivalent to the version in the paper?
      val K_prime = List(lkg.self) ++ K
      val L_S_prime_prime = computeTypLinkage(K_prime, Sp(lkg.self))
      exhaustivityCheck(K_prime, L_S_prime_prime)
    }}
  }


  // Well-formedness of types
  def wfType(K: PathCtx, t: Type): Boolean = t match {
    case NType => true
    case BType => true
    case FunType(input, output) => wfType(K, input) && wfType(K, output)
    case PathType(path, name) =>
      val linkage = computeTypLinkage(K, path.get)
      linkage.types.contains(name) || linkage.adts.contains(name)
    case RecType(fields) =>
      fields.forall { (name, t) => wfType(K, t) }
  }
}