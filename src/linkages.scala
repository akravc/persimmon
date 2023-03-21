import PersimmonSyntax.*
import TestDefParser._

object PersimmonLinkages {

    /* ======================== Linkage Computation ======================== */
    
    // L-Prog-Typ
    def computeLProgTyp(K: PathCtx, p: String): TypingLinkage = {
        // TODO: do type-level parsing here
        null
    }

    // L-Prog-Def
    def computeLProgDef(K: PathCtx, p: String): DefinitionLinkage = {
        // if parsing successful
        if (canParse(pProgram, p)) {
            // return what was parsed
            parseSuccess(pProgram, p)
        } else {
            throw new Exception("L-Prog-Def: Cannot parse the program.")
        }
    }

    // L-Self
    def computeLSelf(K: PathCtx, a: Sp): Linkage = {
        // can assume shape self(a.A) for path
        a match {
            case Sp(SelfFamily(pref, fam)) => 
                if (K.contains(a)) {
                    computeLNest(K, AbsoluteFamily(pref, fam))
                } else throw new Exception("L-Self: Path is not in scope.")
            case _ => throw new Exception("L-Self: Path shape is incorrect.")
        }
    }

    // L-Sub
    def computeLSub(K: PathCtx, a: AbsoluteFamily) = {
        null
    }

    // L-Nest
    def computeLNest(K: PathCtx, a: AbsoluteFamily): Linkage = {
        null
    }

    // Universal computation function for linkages
    // Option 1 computes typing linkage, 
    // Option 2 computes definition linkage
    def computeLinkage(K: PathCtx, a: Path, option: Int): Linkage = {
        TypingLinkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map())
    }
    
    // user friendly computation functions with built-in casting
    def computeTypLinkage(K: PathCtx, a: Path): TypingLinkage = {
        computeLinkage(K, a, 1).asInstanceOf[TypingLinkage]
    }

    def computeDefLinkage(K: PathCtx, a: Path): DefinitionLinkage = {
        computeLinkage(K, a, 2).asInstanceOf[DefinitionLinkage]
    }

    /* ======================== Path Substitution ======================== */

    /* ====================== Linkage Concatenation ====================== */

}