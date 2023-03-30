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
                    case _ => opt match { 
                        // no parent, so return dummy empty linkage
                        case LinkageType.DefLink => 
                            DefinitionLinkage(null, null, null, Map(), Map(), Map(), Map(), Map(), Map())
                        case LinkageType.TypLink => 
                            TypingLinkage(null, null, null, Map(), Map(), Map(), Map(), Map())
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

    // Rule CAT-TOP
    def concatenateLinkages(lkgSuper: Linkage, lkgExt: Linkage): Linkage = {
        // update paths in inherited code so that they refer to the extension
        var lkgP = pathSub(lkgSuper, Sp(lkgExt.getSelfPath()), Sp(lkgSuper.getSelfPath()))

        (lkgP, lkgExt) match {
            // concat typing linkages
            case (TypingLinkage(p1, sp1, sup1, types1, adts1, funs1, cases1, nested1), TypingLinkage(p2, sp2, sup2, types2, adts2, funs2, cases2, nested2)) =>
                TypingLinkage(
                    path = p2,
                    self = sp2,
                    sup = sup2,
                    types = concatTypes(types1, types2),
                    adts = concatADTS(adts1, adts2),
                    funs = concatFunSigs(funs1, funs2),
                    cases = concatCasesSigs(cases1, cases2),
                    nested = concatNestedLinkages(nested1, nested2).asInstanceOf[Map[String, TypingLinkage]]
                )
            // concat definition linkages
            case (DefinitionLinkage(p1, sp1, sup1, types1, defaults1, adts1, funs1, cases1, nested1), DefinitionLinkage(p2, sp2, sup2, types2, defaults2, adts2, funs2, cases2, nested2)) =>
                DefinitionLinkage(
                    path = p2,
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
        nest1 // TODO
    }

    // Rule CAT-TYPES
    def concatTypes(types1: Map[String, TypeDefn], types2:Map[String, TypeDefn]): Map[String, TypeDefn] = {
        types1 // TODO
    }

    // Rule CAT-ADTS
    def concatADTS(adts1: Map[String, AdtDefn], adts2: Map[String, AdtDefn]): Map[String, AdtDefn] = {
        adts1 // TODO
    }

    // Rule CAT-DEFAULTS
    def concatDefaults(defs1: Map[String, DefaultDefn], defs2: Map[String, DefaultDefn]): Map[String, DefaultDefn] = {
        defs1 // TODO
    }

    // Rule CAT-FUNS-TYP
    def concatFunSigs(funs1: Map[String, FunSig], funs2: Map[String, FunSig]): Map[String, FunSig] = {
        funs1 // TODO
    }
    
    // Rule CAT-CASES-TYP
    def concatCasesSigs(cases1: Map[String, CasesSig], cases2: Map[String, CasesSig]): Map[String, CasesSig] = {
        cases1 // TODO
    }

    // Rule CAT-FUNS-DEF
    def concatFunDefns(funs1: Map[String, FunDefn], funs2: Map[String, FunDefn]): Map[String, FunDefn] = {
        funs1 // TODO
    }

    // Rule CAT-CASES-DEF
    def concatCasesDefns(cases1: Map[String, CasesDefn], cases2: Map[String, CasesDefn]): Map[String, CasesDefn] = {
        cases1 // TODO
    }
}