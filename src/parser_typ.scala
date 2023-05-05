import scala.util.parsing.combinator.*
import scala.annotation.tailrec
import PersimmonSyntax.*


class PersimmonTypParser extends RegexParsers with PackratParsers {

  def hasDuplicateName[K, V](kvList: List[(K, V)]): Boolean = 
    kvList.size != kvList.distinctBy(_._1).size

  def unSnoc[T](list: List[T]): (List[T], T) = list match {
    case Nil => throw new Exception("Cannot unSnoc an empty list")
    case List(x) => (List(), x)
    case x :: xs =>
      val (us, u) = unSnoc(xs)
      (x::us, u)
  }

  def between[T, A, B](l: Parser[A], r: Parser[B], mid: Parser[T]): Parser[T] = l ~> mid <~ r
  def optBetween[T, A, B](l: Parser[A], r: Parser[B], mid: Parser[T]): Parser[T] = between(l, r, mid) | mid

  // KEYWORDS
  val kwMatch: Parser[String] = "match\\b".r
  val kwWith: Parser[String] = "with\\b".r
  val kwTrue: Parser[String] = "true\\b".r
  val kwFalse: Parser[String] = "false\\b".r
  val kwLam: Parser[String] = "lam\\b".r
  val kwType: Parser[String] = "type\\b".r
  val kwVal: Parser[String] = "val\\b".r
  val kwFamily: Parser[String] = "Family\\b".r
  val kwExtends: Parser[String] = "extends\\b".r
  val kwN: Parser[String] = "N\\b".r
  val kwB: Parser[String] = "B\\b".r
  val kwString: Parser[String] = "String\\b".r
  val kwSelf: Parser[String] = "self\\b".r
  val kwCases: Parser[String] = "cases\\b".r
  val kwIf: Parser[String] = "if\\b".r
  val kwThen: Parser[String] = "then\\b".r
  val kwElse: Parser[String] = "else\\b".r
  val kwDef: Parser[String] = "def\\b".r
  val kwCase: Parser[String] = "case\\b".r

  val reserved: Parser[String] = kwMatch | kwWith | kwTrue | kwFalse 
    | kwLam | kwType | kwVal | kwFamily | kwExtends | kwN | kwB | kwString 
    | kwSelf | kwCases | kwIf | kwThen | kwElse | kwDef | kwCase

  // NAMES
  lazy val pVarName: Parser[String] = 
    not(reserved) ~> """_|[a-z][a-zA-Z0-9_]*""".r
  lazy val pFamilyName: Parser[String] = 
    not(reserved) ~> """([A-Z][a-zA-Z0-9_]*)+""".r
  lazy val pTypeName: Parser[String] = 
    not(reserved) ~> """([A-Z][a-z]*)+""".r
  lazy val pFunctionName: Parser[String] = 
    not(reserved) ~> """[a-z][a-zA-Z_0-9]*""".r
  // field names can also be constructor names or underscores because of cases
  // is this a problem to allow this for all records?
  lazy val pFieldName: Parser[String] = 
    not(reserved) ~> """(([a-z0-9])+)|(([A-Z][a-zA-Z0-9_]*)+)|_""".r
  lazy val pConstructorName: Parser[String] = 
    not(reserved) ~> """[A-Z][a-zA-Z0-9_]*""".r

  // FAMILY PATHS
  lazy val pPath: PackratParser[Path] =
    pAbsoluteFamPath ^^ {f => f}
    | pSelfPath ^^ { Sp.apply }

  lazy val pAbsoluteFamPath: PackratParser[AbsoluteFamily] =
    pPath ~ ("." ~> pFamilyName) ^^ { case p~f => AbsoluteFamily(p, f) }
    | pFamilyName ^^ { f => AbsoluteFamily(Sp(Prog), f) }

  lazy val pSelfPath: PackratParser[SelfPath] =
    kwSelf ~> between("(", ")",
      pSelfPath ~ ("." ~> pFamilyName) ^^ { case p~f => SelfFamily(Sp(p), f) }
      | pFamilyName ^^ { f => SelfFamily(Sp(Prog), f) }
    )

  // This is needed for things of the form `path.(family/type)name`,
  // since when `path` is absolute, we get something like `[self(A).]C.D.T`
  // and `pPath` itself cannot tell when to stop (will consume `T`)
  lazy val pPathExtra: PackratParser[(Path, String)] =
    // Absolute path prefix (note that this can start with a single self(...))
    (pSelfPath <~ ".").? ~ pFamilyName ~ ("." ~> rep1sep(pFamilyName, ".")) ^^ {
      case _~_~Nil => throw new Exception("Should be impossible")
      case optSelfHead~n~ns =>
        // n :: ns has length at least 2
        val (namesInit, namesLast) = unSnoc(n::ns)
        val inner: Path = Sp(optSelfHead.getOrElse(Prog))
        (namesInit.foldLeft(inner) { AbsoluteFamily.apply }, namesLast)
    }
    // Self path prefix
    | pSelfPath ~ ("." ~> pFamilyName) ^^ {
      case sp~n => (Sp(sp), n)
    }

  // TYPES
  lazy val pFunType: PackratParser[FunType] = pType ~ ("->" ~> pType) ^^ { case inp~out => FunType(inp, out) }
  lazy val pRecField: PackratParser[(String, Type)] = pFieldName ~ (":" ~> pType) ^^ { case f~t => f->t }
  lazy val pRecType: PackratParser[RecType] = between("{", "}", repsep(pRecField, ",") ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record type with duplicate fields.")
      else RecType(lst.toMap)
  })
  lazy val pFamType: PackratParser[PathType] =
    pPathExtra ^^ { case (p,t) => PathType(Some(p), t) }
    | pTypeName ^^ { t => PathType(None, t) } // TODO: where do we fill these in later on? "None" needs to become selfpath

  lazy val pNType: PackratParser[Type] = kwN ^^^ NType
  lazy val pBType: PackratParser[Type] = kwB ^^^ BType

  // separate parser for record field definition with defaults
  lazy val pDefaultRecField: PackratParser[(String, Type)] =
    pFieldName ~ (":" ~> pType) ~ ("=" ~> pExp).? ^^ { case f~t~_ => f -> t }
  // separate parser for record type definition with defaults
  lazy val pDefaultRecType: PackratParser[RecType] = "{"~> repsep(pDefaultRecField, ",") <~"}" ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record type with duplicate fields.")
      else { 
        val type_fields = lst.collect{case (s, t) => (s, t)}.toMap;
        RecType(type_fields) }
  }

  lazy val pType: PackratParser[Type] = pFunType | pRecType | pNType | pBType 
    | pFamType | between("(", ")", pType)

  // ADTS
  lazy val pAdtConstructor: PackratParser[(String, RecType)] = pConstructorName ~ pRecType ^^ { case k ~ v => k -> v }
  lazy val pAdt: PackratParser[AdtDefn] =
    (kwType ~> pTypeName) ~ pMarker ~ repsep(pAdtConstructor, "|") ^^ {
      case n~m~cs =>
        if hasDuplicateName(cs) // disallow ADTs with duplicate constructors
        then throw new Exception("Parsing an ADT with duplicate constructors.")
        else AdtDefn(n, m, cs.toMap)
    }

  // EXPRESSIONS
  lazy val pExpBool: PackratParser[BExp] = kwTrue ^^^ BExp(true) | kwFalse ^^^ BExp(false)
  lazy val pExpNat: PackratParser[NExp] = """(0|[1-9]\d*)""".r ^^ { n => NExp(n.toInt) }

  lazy val pExpIfThenElse: PackratParser[IfThenElse] =
    (kwIf ~> pExp) ~ (kwThen ~> pExp) ~ (kwElse ~> pExp) ^^ {
      case condExpr~ifExpr~elseExpr => IfThenElse(condExpr, ifExpr, elseExpr)
    }

  lazy val pExpVar: PackratParser[Var] = pVarName ^^ { id => Var(id) }
  lazy val pExpLam: PackratParser[Lam] =
    kwLam ~> between("(", ")", pExpVar ~ (":" ~> pType)) ~ ("." ~> pExp) ^^ { case v~t~body => Lam(v, t, body) }

  // Implicit self path functions are parsed as Vars first, then resolved later.
  lazy val pExpFamFun: PackratParser[FamFun] =
    pPath ~ ("." ~> pFunctionName) ^^ { case p~n => FamFun(Some(p), n) }

  lazy val pExpFamCases: PackratParser[FamCases] =
    between("<", ">", (pPath <~ ".").? ~ pFunctionName) ^^ { case p~n => FamCases(p, n) }

  lazy val pExpApp: PackratParser[App] = pExp ~ pExp ^^ { case e~g => App(e, g) }
  lazy val pExpProj: PackratParser[Proj] = pExp ~ "." ~ pFieldName ^^ {case e~_~n => Proj(e, n)}
  lazy val pFieldVal: PackratParser[(String, Expression)] = pFieldName ~ "=" ~ pExp ^^ {case k~_~v => k -> v}
  lazy val pExpRec: PackratParser[Rec] = "{"~> repsep(pFieldVal, ",") <~"}" ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record with duplicate fields.")
      else Rec(lst.toMap)
  }

  lazy val pExpInst: PackratParser[Inst] =
    pFamType ~ between("(", ")", pExpRec) ^^ { case t~r => Inst(t, r) }
  lazy val pExpInstAdt: PackratParser[InstADT] =
    pFamType ~ between("(", ")", pConstructorName ~ pExpRec) ^^ { case t~(c~r) => InstADT(t, c, r) }

  lazy val pExpMatch: PackratParser[Match] =
    kwMatch ~> pExp ~ (kwWith ~> pExpFamCases) ~ pExpRec ^^ { case e~c~r => Match(e, c, r) }

  lazy val pExpExtendedApp: PackratParser[Expression] =
    pExp ~ ("(" ~> repsep(pExp, ",") <~ ")") ^^ {
      case e~gs => gs.foldLeft(e)(App.apply)
    }

  lazy val pExp: PackratParser[Expression] = 
    pExpProj | pExpMatch | pExpInstAdt | pExpInst | pExpApp | pExpRec
    | pExpExtendedApp
    | pExpIfThenElse | pExpLam | pExpBool | pExpNat
    | pExpFamFun | pExpFamCases
    | pExpVar
    | between("(", ")", pExp)

  // MARKERS
  lazy val pMarker: PackratParser[Marker] =
    "=" ^^ {_ => Eq} | "+=" ^^ {_ => PlusEq}

  // DEFINITIONS
  lazy val pTypeDef: PackratParser[(String, (Marker, RecType))] =
    kwType ~> pTypeName ~ pMarker ~ pDefaultRecType ^^ { case n~m~rt => n -> (m -> rt) }
  lazy val pAdtDef: PackratParser[(String, AdtDefn)] =
    pAdt ^^ { a => a.name -> a }

  lazy val pFunDef: PackratParser[(String, FunSig)] =
    kwVal ~> pFunctionName ~ (":" ~> optBetween("(", ")", pFunType)) ~ ("=" ~> pExp) ^^ {
      case n~t~_ => n -> FunSig(n, t)
    }

  lazy val pMatchType: PackratParser[PathType] = between("<", ">", pFamType)
  // mt = match type, m = marker, ft = funtype, lam = body
  lazy val pCasesDef: PackratParser[(String, CasesSig)] =
    kwCases ~> pFunctionName ~ pMatchType ~ (":" ~> optBetween("(", ")", pFunType)) ~ pMarker ~ pExp ^^ {
      case n~mt~ft~m~_ => n -> CasesSig(n, mt, m, ft)
    }

  val cases_suffix = "_cases"
  type ExtendedDef = (Option[FunSig], CasesSig)
  case class ExtendedDefCase(constructor: String, params: List[(String, Type)])
  def extendedDef(name: String, params: List[(String, Type)], matchType: PathType, returnType: Type, marker: Marker, bodies: List[ExtendedDefCase])
  : PackratParser[(String, ExtendedDef)] = {
    if (hasDuplicateName(params)) failure(s"duplicate name in $params")
    else if (hasDuplicateName(bodies.map{(_.constructor -> 0)})) failure("duplicate constructor")
    else {
      val name_cases = name+cases_suffix
      val x = Var("$x")
      val matched_var = Var("$m")
      val casesType = RecType(bodies.map{c => (c.constructor -> FunType(RecType(c.params.toMap), returnType))}.toMap)
      val inputType = RecType(params.toMap)
      val foldedType = params.foldRight(FunType(matchType, returnType)){
        case ((p,t),r) => FunType(t, r)}
      val t = FunType(inputType, casesType)
      val funSig = marker match {
        case Eq => Some(FunSig(name, foldedType))
        case PlusEq => None
      }
      val casesSig = CasesSig(name_cases, matchType, marker, t)
      success(name -> (funSig, casesSig))
    }
  }
  def extendedDefCase(constructor: String, params: List[(String, Type)]): PackratParser[ExtendedDefCase] = {
    if (hasDuplicateName(params))
      failure(s"duplicate names in $params")
    else success(ExtendedDefCase(constructor, params))
  }

  lazy val pExtendedDef: PackratParser[(String, ExtendedDef)] =
    kwDef ~> pFunctionName ~ (("(" ~> repsep(pRecField, ",") <~ ")") | success(Nil)) ~ (":" ~> optBetween("(", ")", (pFamType ~ ("->" ~> pType)))) ~ pMarker >> {
      case n~p~(f~t)~m => repsep(pExtendedDefCase, "") >> {bs =>
        extendedDef(n, p, f, t, m, bs)
      }
    }

  lazy val pExtendedDefCase: PackratParser[ExtendedDefCase] =
    (kwCase ~> pConstructorName ~ ("(" ~> repsep(pRecField, ",") <~ ")" <~ "=") ~ pExp >> {
      case c~p~_ => extendedDefCase(c, p)
    }) | (kwCase ~> "_" ~> "=" ~> pExp >> {_ => extendedDefCase("_", Nil)})

  // A family can extend another family. If it does not, the parent is None.
  def pFamDef(selfPrefix: SelfPath): PackratParser[(String, TypingLinkage)] = {
    for {
      fam <- kwFamily ~> pFamilyName
      curSelfPath = SelfFamily(Sp(selfPrefix), fam)
      supFam <- (kwExtends ~> pAbsoluteFamPath).?
      typs~adts~funs0~extended~cases0~mixins~nested <- between("{", "}",
        rep(pTypeDef) ~ rep(pAdtDef) ~ rep(pFunDef) ~ rep(pExtendedDef) ~ rep(pCasesDef) ~
        rep(pMixDef(curSelfPath)) ~ rep(pFamDef(curSelfPath))
      )
    } yield {
      val funs = funs0 ++ extended.filter{_._2._1.nonEmpty}.map{(k,v) => (k -> v._1.get)}
      val cases = cases0 ++ extended.map{(k,v) => (k+cases_suffix -> v._2)}
      val new_nested = mixins ++ nested

      if hasDuplicateName(typs) then throw new Exception("Parsing duplicate type names.")
      else if hasDuplicateName(adts) then throw new Exception("Parsing duplicate ADT names.")
      else if hasDuplicateName(funs) then throw new Exception("Parsing duplicate function names.")
      else if hasDuplicateName(cases) then throw new Exception("Parsing duplicate cases names.")
      else if hasDuplicateName(new_nested) then throw new Exception("Parsing duplicate family names.")
      else {
        val typedefs = typs.map { 
          case (s, (m, rt)) => s -> TypeDefn(s, m, rt) }.toMap
        
        fam -> TypingLinkage(
          Sp(curSelfPath),
          supFam,
          typedefs,
          adts.toMap,
          funs.toMap,
          cases.toMap,
          new_nested.toMap
        )
      }
    }
  }
  
  // TODO: Finish implementing mixin parsing.
  def pMixDef(selfPrefix: SelfPath): PackratParser[(String, TypingLinkage)] = {
    for {
      fam <- kwFamily ~> pFamilyName
      curSelfPath = SelfFamily(Sp(selfPrefix), fam)
      baseSelfPath = SelfFamily(Sp(curSelfPath), "#Base")
      derivedSelfPath = SelfFamily(Sp(curSelfPath), "#Derived")
      supFam <- (kwExtends ~> pAbsoluteFamPath).?
      typs~adts~funs0~extended~cases0~mixins~nested <- between("{", "}",
        rep(pTypeDef) ~ rep(pAdtDef) ~ rep(pFunDef) ~ rep(pExtendedDef) ~ rep(pCasesDef) ~
        rep(pMixDef(baseSelfPath)) ~ rep(pFamDef(baseSelfPath))
      )
    } yield {
      val funs = funs0 ++ extended.filter{_._2._1.nonEmpty}.map{(k,v) => (k -> v._1.get)}
      val cases = cases0 ++ extended.map{(k,v) => (k+cases_suffix -> v._2)}
      val new_nested = mixins ++ nested
      
      if hasDuplicateName(typs) then throw new Exception("Parsing duplicate type names.")
      else if hasDuplicateName(adts) then throw new Exception("Parsing duplicate ADT names.")
      else if hasDuplicateName(funs) then throw new Exception("Parsing duplicate function names.")
      else if hasDuplicateName(cases) then throw new Exception("Parsing duplicate cases names.")
      else if hasDuplicateName(new_nested) then throw new Exception("Parsing duplicate family names.")
      else {
        val typedefs = typs.map { 
          case (s, (m, rt)) => s -> TypeDefn(s, m, rt) }.toMap
        
        fam -> TypingLinkage(
          Sp(curSelfPath),
          None, Map(), Map(), Map(), Map(),
          Map(
            "#Base" -> TypingLinkage(
              Sp(baseSelfPath),
              supFam,
              typedefs,
              adts.toMap,
              funs.toMap,
              cases.toMap,
              nested.toMap
            ),
            "#Derived" -> TypingLinkage(
              Sp(derivedSelfPath),
              Some(AbsoluteFamily(Sp(curSelfPath), "#Base")), // TODO: Is this right?
              Map(), Map(), Map(), Map(), Map(),
            ),
          ),
        )
      }
    }
  }

  lazy val pProgram: PackratParser[TypingLinkage] =
    rep(pFamDef(Prog)) ^^ {
      fams => 
        TypingLinkage(Sp(Prog), None, Map(), Map(), Map(), Map(),
          fams.toMap
        )
    }

  // Simple preprocessing to remove eol comments
  def removeComments(s: String): String = {
    @tailrec
    def removeCommentsList(inp: List[Char], acc: List[Char]): List[Char] = inp match {
      case Nil => acc
      case '/' :: '/' :: restInp => removeCommentsList(restInp.dropWhile(_ != '\n'), acc)
      case i :: is => removeCommentsList(is, i::acc)
    }

    removeCommentsList(s.toList, Nil).reverse.mkString
  }
}

object TestTypParser extends PersimmonTypParser {
  def parse0Typ[T](p: PackratParser[T], inp: String): ParseResult[T] = parseAll(phrase(p), removeComments(inp))
  def canParseTyp[T](p: PackratParser[T], inp: String): Boolean = parse0Typ(p, inp).successful
  def parseSuccessTyp[T](p: PackratParser[T], inp: String): T = parse0Typ(p, inp).get
}
