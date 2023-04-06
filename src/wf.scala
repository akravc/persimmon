import PersimmonSyntax.*
import PersimmonTyping.*
import PersimmonLinkages.*

object PersimmonWF {
  // Well-formedness of definitions
  def wfDef(K: PathCtx, d: Definition): Boolean = {
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