import PersimmonSyntax.*
import TestDefParser._
import scala.io.Source

object PersimmonLinkages {

    // class for exceptions
    case class LinkageException(s: String) extends Exception(s)

    /* ======================== Linkage Computation ======================== */

    def readProgram(filename: String): String = {
        Source.fromFile(filename).getLines.mkString
    }
    
    // L-Prog-Typ
    def computeLProgTyp(K: PathCtx): TypingLinkage = {
        // TODO: do type-level parsing here
        null
    }

    // L-Prog-Def
    def computeLProgDef(K: PathCtx): DefinitionLinkage = {
        val p = readProgram("program.txt")
        // if parsing successful
        if (canParse(pProgram, p)) {
            // return what was parsed
            parseSuccess(pProgram, p)
        } else {
            throw new Exception("L-Prog-Def: Cannot parse the program.")
        }
    }

    // L-Self
    def computeLSelf(K: PathCtx, a: Sp, option: Int): Linkage = {
        // can assume shape self(a.A) for path
        a match {
            case Sp(SelfFamily(pref, fam)) => 
                if (K.contains(a)) {
                    computeLNest(K, AbsoluteFamily(pref, fam), option)
                } else throw new Exception("L-Self: Path is not in scope.")
            case _ => throw new Exception("L-Self: Path shape is incorrect.")
        }
    }

    // L-Sub
    def computeLSub(K: PathCtx, a: AbsoluteFamily, option: Int): Linkage = {
        val lkg = computeLNest(K, a, option)
        pathSub(lkg, a, Sp(SelfFamily(a.pref, a.fam)))
    }

    // L-Nest
    def computeLNest(K: PathCtx, a: AbsoluteFamily, option: Int): Linkage = {
        val lkgWrap = computeLinkage(K, a.pref, option)
        if (option == 1) {
            // typing linkage computation
            val lkg = lkgWrap.asInstanceOf[TypingLinkage].nested.get(a.fam)
            lkg match {
                case Some(lkgA) => 
                    val superPath = lkgA.sup
                    val superLkg = superPath match {
                        case Some(p) => computeLNest(K, p, option)
                        case _ => null // no parent linkage to compute
                    }
                    concatenateLinkages(superLkg, lkgA, option)
                case _ => 
                    throw new LinkageException("L-Nest: no nested linkage for family " + a.fam)
            }
        } else if (option == 2) {
            // definition linkage computation
            val lkg = lkgWrap.asInstanceOf[DefinitionLinkage].nested.get(a.fam)
            lkg match {
                case Some(lkgA) => 
                    val superPath = lkgA.sup
                    val superLkg = superPath match {
                        case Some(p) => computeLNest(K, p, option)
                        case _ => null // no parent linkage to compute
                    }
                    concatenateLinkages(superLkg, lkgA, option)
                case _ => 
                    throw new LinkageException("L-Nest: no nested linkage for family " + a.fam)
            }
        } else throw new LinkageException("L-Nest: option not supported.")
    }

    // Universal computation function for linkages
    // Option 1 computes typing linkage, 
    // Option 2 computes definition linkage
    def computeLinkage(K: PathCtx, a: Path, option: Int): Linkage = {
        if (option == 1) {
            // typing linkage computation
            a match {
                // K |- a.A ~> L (L-Sub applies)
                case AbsoluteFamily(pref, fam) => 
                    computeLSub(K, a.asInstanceOf[AbsoluteFamily], option)
                case Sp(sp) => sp match {
                    case Prog => computeLProgTyp(K)
                    case SelfFamily(pref, fam) => 
                }
            }
        } else if (option == 2) {
            null
        } else throw new LinkageException("L-Nest: option not supported.")
    }
    
    // user friendly computation functions with built-in casting
    def computeTypLinkage(K: PathCtx, a: Path): TypingLinkage = {
        computeLinkage(K, a, 1).asInstanceOf[TypingLinkage]
    }

    def computeDefLinkage(K: PathCtx, a: Path): DefinitionLinkage = {
        computeLinkage(K, a, 2).asInstanceOf[DefinitionLinkage]
    }

    /* ======================== Path Substitution ======================== */

    // Substitute path p2 in the linkage with path p1
    // lkg[p1/p2]
    def pathSub(lkg: Linkage, p1: Path, p2: Path): Linkage = {
        null //TODO
    }

    /* ====================== Linkage Concatenation ====================== */

    def concatenateLinkages(lkgSuper: Linkage, lkgExt: Linkage, option: Integer): Linkage = {
        null //TODO
    }

}