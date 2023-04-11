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

  // ancestors function
  def ancestors(p: SelfPath): List[SelfPath] = {
    List()
  }

  // well-formedness of type definitions
  def wfTypDef(K: PathCtx, td: TypeDefn, dd: DefaultDefn): Boolean = {
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