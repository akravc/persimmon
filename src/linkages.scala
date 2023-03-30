import PersimmonSyntax.*
import TestDefParser._
import TestTypParser._
import PrettyPrint._
import scala.io.Source

object PersimmonLinkages {

    /* ======================== Global vars ======================== */

    // TODO: program, cache

    /* ======================== Helpers ======================== */

    // class for exceptions
    case class LinkageException(s: String) extends Exception(s)

    // What kind of linkage are we computing?
    enum LinkageType: 
        case TypLink, DefLink

    // Helper - reads program from file
    def readProgram(filename: String): String = {
        Source.fromFile(filename).getLines.mkString
    }

    /* ===================== Linkage Computation Rules ===================== */
    
    // L-Prog-Typ
    def computeLProgTyp(K: PathCtx): TypingLinkage = {
        val p = readProgram("program.txt")
        // if parsing successful
        if (canParseTyp(TestTypParser.pProgram, p)) {
            // return what was parsed
            parseSuccessTyp(TestTypParser.pProgram, p)
        } else {
            throw new Exception("L-Prog-Def: Cannot parse the program.")
        }
    }

    // L-Prog-Def
    def computeLProgDef(K: PathCtx): DefinitionLinkage = {
        val p = readProgram("program.txt")
        // if parsing successful
        if (canParse(TestDefParser.pProgram, p)) {
            // return what was parsed
            parseSuccess(TestDefParser.pProgram, p)
        } else {
            throw new Exception("L-Prog-Def: Cannot parse the program.")
        }
    }

    // L-Self
    def computeLSelf(K: PathCtx, a: Sp, opt: LinkageType): Linkage = {
        // can assume shape self(a.A) for path
        a match {
            case Sp(SelfFamily(pref, fam)) => 
                if (K.contains(a)) {
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
        val lkg = lkgWrap.getNestedLinkage(a.fam)
        
        lkg match {
            case Some(lkgA) => 
                val superPath = lkgA.getSuperPath()
                val superLkg = superPath match {
                    case Some(p) => computeLNest(K, p, opt)
                    case _ => null // no parent linkage to compute
                }
                concatenateLinkages(superLkg, lkgA, opt)
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
    def pathSub(lkg: Linkage, p1: Path, p2: Path): Linkage = {
        lkg // TODO
    }

    /* ====================== Linkage Concatenation ====================== */

    def concatenateLinkages(lkgSuper: Linkage, lkgExt: Linkage, opt: LinkageType): Linkage = {
        lkgExt // TODO
    }

}