import PersimmonSyntax.*
import PersimmonTyping.*
import PersimmonLinkages.*

object PersimmonWF {

  // Well-formedness of definitions
  // the top level rule recursively checks 
  // the definition linkage for path prog
  def wfDef(K: PathCtx, lkg: DefinitionLinkage): Boolean = {
    true
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
  def ancestors(K: PathCtx, p: SelfPath): List[SelfPath] = {
    var currLkg = computeTypLinkage(K, Sp(p))
    currLkg.getSuperPath() match {
      case Some(p) => relativizePath(p) :: ancestors(K, relativizePath(p))
      case None => return List()
    } 
  }

  // well-formedness of type definitions
  def wfTypDef(K: PathCtx, td: TypeDefn): Boolean = {
    true
  }

  // well-formedness of type extensions and their defaults
  def wfTypDefExt(K: PathCtx, td: TypeDefn, dd: DefaultDefn): Boolean = {
    true
  }

  // well-formedness of ADT definitions
  def wfAdtDef(K: PathCtx, adt: AdtDefn): Boolean = {
    true
  }

  // well-formedness of functions
  def wfFunDef(K: PathCtx, fd: FunDefn): Boolean = {
    true
  }

  // well-formedness of cases definitions
  def wfCasesDef(K: PathCtx, cd: CasesDefn): Boolean = {
    true
  }

  // rule EC-Nest
  def exhaustivityCheck(K: PathCtx, lkg: TypingLinkage): Boolean = {
    true
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