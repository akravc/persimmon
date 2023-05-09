import scala.annotation.tailrec

object PersimmonSyntax {
  /* ======================== FAMILIES & PATHS ======================== */

  // Path a := sp | a.A
  sealed trait Path 
  case class Sp(sp: SelfPath) extends Path // sp 
  case class AbsoluteFamily(pref: Path, fam: String) extends Path // a.A

  // Relative Path sp := prog | self(a.A)
  sealed trait SelfPath
  case object Prog extends SelfPath // prog
  case class SelfFamily(pref: Path, fam: String) extends SelfPath // self(a.A)
  
  //////////////////////// Path helper functions ////////////////////////

  // returns the last family name in the path (suffix)
  // TODO: rename to suffix
  // TODO; prog path should not throw an exception, instead return None.
  def pathName(p: Path): String = p match {
    case Sp(Prog) => 
      throw new Exception("Cannot get suffix of prog path.")
    case Sp(SelfFamily(_, f)) => f
    case AbsoluteFamily(_, f) => f
  }

  // Transforms all self paths into absolute paths (except Prog)
  def concretizePath(p: Path): Path = p match {
    case Sp(Prog) => p
    case Sp(SelfFamily(pref, fam)) => AbsoluteFamily(concretizePath(pref), fam)
    case AbsoluteFamily(pref, fam) => AbsoluteFamily(concretizePath(pref), fam)
  }

  def concretizePath0(p: Path): Path = p match {
    case Sp(SelfFamily(pref, fam)) => AbsoluteFamily(pref, fam)
    case other => other
  }

  // Transforms all absolute paths into self paths
  def relativizePath(p: Path): SelfPath = p match {
    case Sp(sp) => sp
    case AbsoluteFamily(pref, fam) => SelfFamily(Sp(relativizePath(pref)), fam)
  }

  // transform path to list of family names
  @tailrec
  def pathToFamList(p: Path, acc: List[String] = Nil): List[String] = p match {
    case Sp(sp) => selfPathToFamList(sp, acc)
    case AbsoluteFamily(pref, fam) => pathToFamList(pref, fam :: acc)
  }
  
  // transform self path to list of family names
  def selfPathToFamList(sp: SelfPath, acc: List[String] = Nil): List[String] = sp match {
    case Prog => acc
    case SelfFamily(pref, fam) => pathToFamList(pref, fam :: acc)
  }

  /* ======================== TYPES ======================== */
  
  sealed trait Type
  case object NType extends Type // N
  case object BType extends Type // B
  case class PathType(var path: Option[Path], name: String) extends Type // a.R
  case class FunType(input: Type, output: Type) extends Type // T -> T'
  case class RecordType(fields: Map[String, Type]) extends Type // {(f: T)*}


  //////////////////////// Type helper functions ////////////////////////

  // Generic traversal to change the paths
  def mapPathInType(f: Path => Path)(t: Type): Type = t match {
    case PathType(path, name) => PathType(path.map(f), name)
    case FunType(input, output) => FunType(mapPathInType(f)(input), mapPathInType(f)(output))
    case RecordType(fields) => RecordType(fields.view.mapValues(mapPathInType(f)).toMap)
    case _ => t
  }

  // Transforms self paths in types into absolute paths (except Prog)
  def concretizeType(t: Type): Type = mapPathInType(concretizePath)(t)

  /* ======================== EXPRESSIONS  ======================== */

  sealed trait Expression 
  case class NExp(n: Int) extends Expression // n
  case class BExp(b: Boolean) extends Expression // b
  case class Var(id: String) extends Expression // x
  case class Lam(v: Var, t: Type, body: Expression) extends Expression // lam (x: T). body
  case class FamFun(var path: Option[Path], name: String) extends Expression // a.m
  case class FamCases(var path: Option[Path], name: String) extends Expression // a.r
  case class App(e1: Expression, e2: Expression) extends Expression // e g
  case class Record(fields: Map[String, Expression]) extends Expression // {(f = e)*}
  case class Proj(e: Expression, name: String) extends Expression // e.f
  case class Inst(t: PathType, rec: Record) extends Expression // a.R({(f = e)*})
  case class InstADT(t: PathType, cname: String, rec: Record) extends Expression // a.R(C {(f = e)*})
  case class Match(e: Expression, c: FamCases, r: Record) extends Expression // match e with a.c {(f_arg = e_arg)*}
  case class IfThenElse(condExpr: Expression, ifExpr: Expression, elseExpr: Expression) extends Expression

  /* ======================== DEFINITIONS ======================== */

  sealed trait Marker // either += or =
  case object PlusEq extends Marker // type extension marker
  case object Eq extends Marker // type definition marker

  //////////////////////// Definitions and Signatures ////////////////////////

  // Things that could be defined or extended / further bound
  // case class DefnBody[B](defn: Option[B], extendsFrom: Option[Path], furtherBindsFrom: Option[Path], allDefns: List[B])
  // def DefnBody[B](defn: Option[B], extendsFrom: Option[Path], furtherBindsFrom: Option[Path]): DefnBody[B] = {
  //   val allDefns: List[B] = defn match {
  //     case None => List()
  //     case Some(x) => List(x)
  //   }
  //   DefnBody[B](defn, extendsFrom, furtherBindsFrom, allDefns)
  // }

  sealed trait Definition
  // types
  case class TypeDefn(name: String, marker: Marker, typeBody: RecordType) extends Definition
  
  // defaults
  case class DefaultDefn(name: String, marker: Marker, defaultBody: Record) extends Definition

  // ADTs
  case class AdtDefn(name: String, marker: Marker, adtBody: Map[String, RecordType]) extends Definition

  // Functions
  case class FunDefn(name: String, t: FunType, funBody: Lam) extends Definition

  // Cases
  // case class CasesDefn(name: String, matchType: PathType, t: FunType, ts: List[Type], marker: Marker, casesBody: DefnBody[Expression]) extends Definition
  // def CasesDefn(name: String, matchType: PathType, t: FunType, marker: Marker, casesBody: DefnBody[Expression]): CasesDefn =
  //   CasesDefn(name, matchType, t, List(t), marker, casesBody)

  case class CasesDefn(name: String, matchType: PathType, t: FunType, ts: List[Type], marker: Marker, casesBody: Expression) extends Definition
  def CasesDefn(name: String, matchType: PathType, t: FunType, marker: Marker, casesBody: Expression): CasesDefn =
    CasesDefn(name, matchType, t, List(t), marker, casesBody)

  
  sealed trait Signature

  // function signature
  case class FunSig(name: String, t: FunType) extends Signature

  // cases signature
  case class CasesSig(name: String, matchType: PathType, marker: Marker, t: FunType) extends Signature

  /* ======================== LINKAGE SYNTAX ======================== */

  sealed trait Linkage {

    // retrieve self path from linkage
    def getSelfPath(): Path = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => self
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => self
        }
    }

    // retrieve super path from linkage
    def getSuperPath(): Option[AbsoluteFamily] = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => sup
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => sup
        }
    }

    // retrieve nested linkage for family "fam", if it exists
    def getNestedLinkage(fam: String): Option[Linkage] = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => nested.get(fam)
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => nested.get(fam) 
        }
    }

    def getAllNested(): Map[String, Linkage] = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => nested
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => nested
        }
    }

    def getTypes(): Map[String, TypeDefn] = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => types
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => types
        }
    }

    def getDefaults(): Option[Map[String, DefaultDefn]] = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => Some(defaults)
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => None
        }
    }

    def getAdts(): Map[String, AdtDefn] = {
        this match {
            case DefinitionLinkage(self, sup, types, defaults, adts, funs, cases, nested) => adts
            case TypingLinkage(self, sup, types, adts, funs, cases, nested) => adts
        }
    }
  }
  
  // This version of the linkage 
  // holds only information needed for typing
  // -- NO definitions
  case class TypingLinkage(
    self: Path, // self
    sup: Option[AbsoluteFamily], // super
    types: Map[String, TypeDefn],
    adts: Map[String, AdtDefn],
    // function signature only
    funs: Map[String, FunSig],
    // cases signature only
    cases: Map[String, CasesSig],
    nested: Map[String, TypingLinkage]
  ) extends Linkage

  // This version of the linkage holds
  // all information including definitions
  case class DefinitionLinkage(
    self: Path, // self
    sup: Option[AbsoluteFamily], // super
    types: Map[String, TypeDefn],
    defaults: Map[String, DefaultDefn],
    adts: Map[String, AdtDefn],
    funs: Map[String, FunDefn],
    cases: Map[String, CasesDefn],
    nested: Map[String, DefinitionLinkage]
  ) extends Linkage
  

  /* ======================== Values ======================== */

  // Values
  def isValue(e: Expression): Boolean = e match {
    case NExp(n) => true
    case BExp(b) => true
    case Lam(v, t, body) => true
    case Inst(t, rec) => rec.fields.forall { case (_, exp) => isValue(exp) }
    case InstADT(t, cname, rec) => rec.fields.forall { case (_, exp) => isValue(exp) }
    case Record(fields) => fields.forall { case (_, exp) => isValue(exp) }
    case _ => false
  }

    /* ======================== Contexts ======================== */

    type TypingCtx = Map[String, Type] // Gamma
    type PathCtx = List[SelfPath] // K
}