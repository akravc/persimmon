import scala.util.parsing.combinator.*
import scala.annotation.tailrec
import PersimmonSyntax.*
import PersimmonUtil.*

class PersimmonParser extends RegexParsers with PackratParsers {

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
  val kwMixin: Parser[String] = "Mixin\\b".r
  val kwExtends: Parser[String] = "extends\\b".r
  val kwN: Parser[String] = "N\\b".r
  val kwB: Parser[String] = "B\\b".r
  val kwStr: Parser[String] = "Str\\b".r
  val kwSelf: Parser[String] = "self\\b".r
  val kwCases: Parser[String] = "cases\\b".r
  val kwIf: Parser[String] = "if\\b".r
  val kwThen: Parser[String] = "then\\b".r
  val kwElse: Parser[String] = "else\\b".r
  val kwDef: Parser[String] = "def\\b".r
  val kwCase: Parser[String] = "case\\b".r

  val reserved: Parser[String] = kwMatch | kwWith | kwTrue | kwFalse 
    | kwLam | kwType | kwVal | kwFamily | kwExtends | kwN | kwB | kwStr
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
  lazy val pFieldName: Parser[String] = 
    not(reserved) ~> """(([a-z0-9])+)|(([A-Z][a-zA-Z0-9_]*)+)|_""".r
  lazy val pConstructorName: Parser[String] = 
    not(reserved) ~> """[A-Z][a-zA-Z0-9_]*""".r
  lazy val pString: Parser[String] = 
    not(reserved) ~> """[^\"]*""".r


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
  lazy val pRecType: PackratParser[RecordType] = between("{", "}", repsep(pRecField, ",") ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record type with duplicate fields.")
      else RecordType(lst.toMap)
  })
  lazy val pFamType: PackratParser[PathType] =
    pPathExtra ^^ { case (p,t) => PathType(Some(p), t) }
    | pTypeName ^^ { t => PathType(None, t) }

  lazy val pNType: PackratParser[Type] = kwN ^^^ NType
  lazy val pBType: PackratParser[Type] = kwB ^^^ BType
  lazy val pStrType: PackratParser[Type] = kwStr ^^^ StrType

  // separate parser for record field definition with defaults
  lazy val pDefaultRecField: PackratParser[(String, (Type, Option[Expression]))] =
    pFieldName ~ (":" ~> pType) ~ ("=" ~> pExp).? ^^ { case f~t~oe => f->(t->oe) }
  // separate parser for record type definition with defaults
  lazy val pDefaultRecType: PackratParser[(RecordType, Record)] = "{"~> repsep(pDefaultRecField, ",") <~"}" ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record type with duplicate fields.")
      else {
        val type_fields = lst.collect{case (s, (t, _)) => (s, t)}.toMap;
        val defaults = lst.collect{case (s, (t, Some(e))) => (s, e)}.toMap;
        RecordType(type_fields) -> Record(defaults)
      }
  }

  lazy val pType: PackratParser[Type] = pFunType | pRecType | pNType | pBType 
    | pStrType | pFamType | between("(", ")", pType)

  // ADTS
  lazy val pAdtConstructor: PackratParser[(String, RecordType)] = pConstructorName ~ pRecType ^^ { case k ~ v => k -> v }
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

  lazy val pExpStr: PackratParser[StrExp] = 
    between("\"", "\"", pString) ^^ {s => StrExp(s)}

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
    (pPath <~ ".").? ~ pFunctionName ^^ { case p~n => FamCases(p, n) }

  lazy val pExpApp: PackratParser[App] = pExp ~ pExp ^^ { case e~g => App(e, g) }
  lazy val pExpPlus: PackratParser[Plus] = pExp ~ "+" ~ pExp ^^ { case e~_~g => Plus(e, g) }
  lazy val pExpMul: PackratParser[Mul] = pExp ~ "*" ~ pExp ^^ { case e~_~g => Mul(e, g) }
  lazy val pExpNeg: PackratParser[Neg] = "-" ~> pExp ^^ { case e => Neg(e) }

  lazy val pExpProj: PackratParser[Proj] = pExp ~ "." ~ pFieldName ^^ {case e~_~n => Proj(e, n)}
  lazy val pFieldVal: PackratParser[(String, Expression)] = pFieldName ~ "=" ~ pExp ^^ {case k~_~v => k -> v}
  lazy val pExpRec: PackratParser[Record] = "{"~> repsep(pFieldVal, ",") <~"}" ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record with duplicate fields.")
      else Record(lst.toMap)
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
    pExpProj | pExpMatch | pExpInstAdt | pExpInst | pExpPlus
    | pExpMul | pExpNeg
    | pExpApp
    | pExpRec
    | pExpExtendedApp
    | pExpIfThenElse | pExpLam | pExpBool | pExpNat
    | pExpFamFun 
    | pExpVar
    | pExpStr
    | pExpFamCases
    | between("(", ")", pExp)

  // MARKERS
  lazy val pMarker: PackratParser[Marker] =
    "=" ^^ {_ => Eq} | "+=" ^^ {_ => PlusEq}

  // DEFINITIONS
  lazy val pTypeDef: PackratParser[(String, TypeDefaultsDefn)] =
    kwType ~> pTypeName ~ pMarker ~ pDefaultRecType ^^ { case n~m~rt => n -> TypeDefaultsDefn(n, m, rt._1, rt._2) }
  lazy val pAdtDef: PackratParser[(String, AdtDefn)] =
    pAdt ^^ { a => a.name -> a }

  lazy val pFunDef: PackratParser[(String, FunDefn)] =
    kwVal ~> pFunctionName ~ (":" ~> optBetween("(", ")", pFunType)) ~ ("=" ~> pExpLam) ^^ {
      case n~t~b => n -> FunDefn(n, t, b)
    }

  lazy val pMatchType: PackratParser[PathType] = between("<", ">", pFamType)
  // mt = match type, m = marker, ft = funtype, lam = body
  lazy val pCasesDef: PackratParser[(String, CasesDefn)] =
    kwCases ~> pFunctionName ~ pMatchType ~ (":" ~> optBetween("(", ")", pFunType)) ~ pMarker ~ pExp ^^ {
      case n~mt~ft~m~b => n -> CasesDefn(n, mt, ft, m, b)
    }


  // Replaces occurrences of any variable id in s with a projection x.id
  def var2proj(x: Expression, s: Set[String])(e: Expression): Expression = {
    val f = var2proj(x, s)
    e match {
      case Var(id) if s.contains(id) => Proj(x, id)
      case Lam(v, t, body) => Lam(v, t, f(body))
      case App(e1, e2) => App(f(e1), f(e2))
      case Plus(e1, e2) => Plus(f(e1), f(e2))
      case Mul(e1, e2) => Mul(f(e1), f(e2))
      case Neg(e) => Neg(f(e))
      case Record(fields) => Record(fields.mapValues(f).toMap)
      case Proj(e, name) => Proj(f(e), name)
      case Inst(t, rec) => Inst(t, f(rec).asInstanceOf[Record])
      case InstADT(t, cname, rec) => InstADT(t, cname, f(rec).asInstanceOf[Record])
      case Match(e, c, r) => Match(f(e), f(c).asInstanceOf[FamCases], f(r).asInstanceOf[Record])
      case IfThenElse(a, b, c) => IfThenElse(f(a), f(b), f(c))
      case _ => e
    }
  }

  val cases_suffix = "_cases"
  case class ExtendedDefCase(constructor: String, params: List[(String, Type)], body: Expression)
  def extendedDef(name: String, params: List[(String, Type)], matchType: PathType, returnType: Type, marker: Marker, bodies: List[ExtendedDefCase]): PackratParser[(String, ExtendedDefn)] = {
    if (hasDuplicateName(params)) failure(s"duplicate name in $params")
    else if (hasDuplicateName(bodies.map{(_.constructor -> 0)})) failure("duplicate constructor")
    else {
      val name_cases = name+cases_suffix
      val x = Var("$x")
      val matched_var = Var("$m")
      val casesType = RecordType(bodies.map{c => (c.constructor -> FunType(RecordType(c.params.toMap), returnType))}.toMap)
      val inputType = RecordType(params.toMap)
      val foldedType = params.foldRight(FunType(matchType, returnType)){
        case ((p,t),r) => FunType(t, r)}
      val t = FunType(inputType, casesType)
      val fun = marker match {
        case Eq => {
          val body0 = Lam(matched_var, matchType, Match(matched_var,
            FamCases(None, name_cases), Record(params.map{(k,_) => (k -> Var("_"+k))}.toMap)))
          val paramsTr = params.map{(k,v) => ("_"+k, v)}.toMap
          val body = paramsTr.foldRight(body0){case ((p,t),r) =>
            Lam(Var(p), t, r)
          }
          Some(FunDefn(name, foldedType, body))
        }
        case PlusEq => None
      }
      val b = Lam(x, inputType, Record(bodies.map{c => c.constructor ->
        var2proj(x, params.map(_._1).toSet)(
          Lam(matched_var, RecordType(c.params.toMap),
            var2proj(matched_var, c.params.map(_._1).toSet)(
              c.body)))}.toMap))
      val cases = CasesDefn(name_cases, matchType, t, marker, b)
      success(name -> ExtendedDefn(name, fun, cases))
    }
  }
  def extendedDefCase(constructor: String, params: List[(String, Type)], body: Expression): PackratParser[ExtendedDefCase] = {
    if (hasDuplicateName(params))
      failure(s"duplicate names in $params")
    else success(ExtendedDefCase(constructor, params, body))
  }

  lazy val pExtendedDef: PackratParser[(String, ExtendedDefn)] =
    kwDef ~> pFunctionName ~ (("(" ~> repsep(pRecField, ",") <~ ")") | success(Nil)) ~ (":" ~> optBetween("(", ")", (pFamType ~ ("->" ~> pType)))) ~ pMarker >> {
      case n~p~(f~t)~m => repsep(pExtendedDefCase, "") >> {bs =>
        extendedDef(n, p, f, t, m, bs)
      }
    }

  lazy val pExtendedDefCase: PackratParser[ExtendedDefCase] =
    (kwCase ~> pConstructorName ~ ("(" ~> repsep(pRecField, ",") <~ ")" <~ "=") ~ pExp >> {
      case c~p~e => extendedDefCase(c, p, e)
    }) | (kwCase ~> "_" ~> "=" ~> pExp >> {e => extendedDefCase("_", Nil, e)})
  
  def pFamBody(curSelfPath: SelfPath, supFam: Option[AbsoluteFamily]): PackratParser[DefinitionLinkage] = {
    for {
      defns <- between("{", "}",
        rep(pTypeDef | pAdtDef | pFunDef | pExtendedDef | pCasesDef | pMixDef(curSelfPath) | pFamDef(curSelfPath))
      )
    } yield {
      val typs = defns.flatMap {(name, defn) => defn match {
        case typ: TypeDefaultsDefn => Some((name, typ))
        case _ => None
      }}
      val adts = defns.flatMap {(name, defn) => defn match {
        case adt: AdtDefn => Some((name, adt))
        case _ => None
      }}
      val funs0 = defns.flatMap {(name, defn) => defn match {
        case fun0: FunDefn => Some((name, fun0))
        case _ => None
      }}
      val extended = defns.flatMap {(name, defn) => defn match {
        case extended: ExtendedDefn => Some((name, extended))
        case _ => None
      }}
      val cases0 = defns.flatMap {(name, defn) => defn match {
        case case0: CasesDefn => Some((name, case0))
        case _ => None
      }}
      val nested = defns.flatMap {(name, defn) => defn match {
        case fams: FamsDefn => fams.linkages
        case _ => List()
      }}
      
      val funs = funs0 ++ extended.filter{_._2.funDefn.nonEmpty}.map{(k,v) => (k -> v.funDefn.get)}
      val cases = cases0 ++ extended.map{(k,v) => (k+cases_suffix -> v.casesDefn)}

      if hasDuplicateName(typs) then throw new Exception("Parsing duplicate type names.")
      else if hasDuplicateName(adts) then throw new Exception("Parsing duplicate ADT names.")
      else if hasDuplicateName(funs) then throw new Exception("Parsing duplicate function names.")
      else if hasDuplicateName(cases) then throw new Exception("Parsing duplicate cases names.")
      else if hasDuplicateName(nested) then throw new Exception("Parsing duplicate family names.")
      else {
        supFam match {
          case Some(b) =>
            // family extends another
            if typs.exists{case (s, typ) => (typ.marker == PlusEq) && (typ.typeBody.fields.keySet != typ.defaultBody.fields.keySet)} then
              throw new Exception("In a type extension, not all fields have defaults.");
            else ()
          // family does not extend another
          case None => ()
        }
        val typedefs = typs.map { (name, typ) => name -> TypeDefn(typ.name, typ.marker, typ.typeBody) }.toMap
        val defaults = typs.collect{ (name, typ) => name -> DefaultDefn(typ.name, typ.marker, typ.defaultBody) }.toMap

        val funHeaders = funs.map { 
          case (s, fundefn) => s -> fundefn.t
        }.toMap
        val casesHeaders = cases.map { 
          case (s, casedefn) => s -> (casedefn.matchType, casedefn.t)
        }.toMap
        
        DefinitionLinkage(
          Sp(curSelfPath),
          supFam,
          typedefs,
          defaults,
          adts.toMap,
          funs.toMap,
          cases.toMap,
          nested.toMap
        )
      }
    }
  }
  
  // Wraps around the 'pFamBody' parser
  def pMixBody(
    curSelfPath: SelfPath, originalSelfPath: SelfPath, supFam: Option[AbsoluteFamily]
  ): PackratParser[DefinitionLinkage] = {
    val baseSelfPath = SelfFamily(Sp(curSelfPath), "#Base")
    val baseAbsolutePath = AbsoluteFamily(Sp(curSelfPath), "#Base")
    val derivedSelfPath = SelfFamily(Sp(curSelfPath), "#Derived")
    for {
      linkage <- pFamBody(derivedSelfPath, Some(baseAbsolutePath))
    } yield {
      val newLinkage = pathSub(linkage, Sp(derivedSelfPath), Sp(originalSelfPath)) match {
        case (_: TypingLinkage) => throw Exception("Should be impossible");
        case (definitionLinkage: DefinitionLinkage) => definitionLinkage
      }
      DefinitionLinkage(
        Sp(curSelfPath),
        None,
        Map(), Map(), Map(), Map(), Map(),
        Map(
          "#Base" -> DefinitionLinkage(
            Sp(baseSelfPath),
            supFam,
            Map(), Map(), Map(), Map(), Map(), Map(),
          ),
          "#Derived" -> newLinkage,
        ),
      )
    }
  }

  // A family can extend another family. If it does not, the parent is None.
  def pFamDef(selfPrefix: SelfPath): PackratParser[(String, FamsDefn)] = {
    for {
      fam <- kwFamily ~> pFamilyName
      supFam <- (kwExtends ~> pAbsoluteFamPath).?
      mixFams <- (kwWith ~> repsep(pAbsoluteFamPath, ",")).?
      auxFams = mixFams match {
        case Some(mixFams) => mixFams.zipWithIndex.map { (mixFam, index) =>
          val auxSelfPath = SelfFamily(Sp(selfPrefix), fam + "#" + index.toString)
          val baseSelfPath = SelfFamily(Sp(auxSelfPath), "#Base")
          (
            fam + "#" + index.toString,
            DefinitionLinkage(
              Sp(auxSelfPath),
              Some(AbsoluteFamily(mixFam.pref, mixFam.fam + "#Mixin")),
              Map(), Map(), Map(), Map(), Map(),
              Map(
                "#Base" -> DefinitionLinkage(
                  Sp(baseSelfPath),
                  if index == 0 then
                    supFam
                  else
                    Some(
                      AbsoluteFamily(
                        AbsoluteFamily(Sp(selfPrefix), fam + "#" + (index-1).toString),
                        "#Derived"
                      )
                    ),
                  Map(), Map(), Map(), Map(), Map(), Map(),
                ),
              ),
            ),
          )
        }
        case None => List()
      }
      curSelfPath = SelfFamily(Sp(selfPrefix), fam)
      linkage <- pFamBody(
        curSelfPath,
        if auxFams.isEmpty then supFam
        else Some(
          AbsoluteFamily(
            AbsoluteFamily(Sp(selfPrefix), fam + "#" + (auxFams.length-1).toString),
            "#Derived"
          )
        ),
      )
    } yield {
      fam -> FamsDefn(fam, auxFams ++ List(fam -> linkage))
    }
  }
  
  def pMixDef(selfPrefix: SelfPath): PackratParser[(String, FamsDefn)] = {
    for {
      mix <- kwMixin ~> pFamilyName
      supFam <- (kwExtends ~> pAbsoluteFamPath).?
      originalSelfPath = SelfFamily(Sp(selfPrefix), mix)
      curSelfPath = SelfFamily(Sp(selfPrefix), mix + "#Mixin")
      linkage <- pMixBody(curSelfPath, originalSelfPath, supFam)
    } yield {
      mix -> FamsDefn(mix, List(
        mix + "#Mixin" -> linkage,
        mix -> DefinitionLinkage(
          Sp(SelfFamily(Sp(selfPrefix), mix)),
          Some(AbsoluteFamily(
            AbsoluteFamily(Sp(selfPrefix), mix + "#Mixin"),
            "#Derived"
          )),
          Map(), Map(), Map(),
          Map(), Map(), Map(),
        )
      ))
    }
  }

  lazy val pProgram: PackratParser[DefinitionLinkage] =
    (rep(pMixDef(Prog) | pFamDef(Prog)) ~ (pExp).?) ^^ { case (famsDefns~oe) =>
      val fams = famsDefns.flatMap {(name, famsDefn) => famsDefn.linkages}
      if hasDuplicateName(fams) then throw new Exception("Parsing duplicate family names.")
      val deflink = DefinitionLinkage(Sp(Prog), None, Map(), Map(), Map(), Map(), Map(), fams.toMap)

      PersimmonProgram.set(deflink, oe)
      deflink
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

object TestParser extends PersimmonParser {
  def parse0[T](p: PackratParser[T], inp: String): ParseResult[T] = parseAll(phrase(p), removeComments(inp))
  def canParse[T](p: PackratParser[T], inp: String): Boolean = parse0(p, inp).successful
  def parseSuccess[T](p: PackratParser[T], inp: String): T = parse0(p, inp).get

  def parseProgramDefLink(inp: String): DefinitionLinkage = {
    val raw = parse0(pProgram, inp).get
    // sub extends paths if needed 
    val subex = fillExtendsPaths(raw, List())
    // infer missing type prefixes
    val filled = fillNonePaths(subex)
    // correct function calls parsed as variable names
    val resolved = resolveFunCalls(filled)
    // unfold wildcards in cases
    val unfolded = unfoldWildcards(resolved)

    // update the program with all pre-processing
    PersimmonProgram.set(unfolded, PersimmonProgram.exp)
    unfolded
  }

  def convertDefToTyp(lkg: DefinitionLinkage): TypingLinkage = {
    TypingLinkage(self = lkg.self, 
                  sup = lkg.sup,
                  types = lkg.types,
                  defaults = lkg.defaults.map {
                    (s, ddef) => (s, ddef.defaultBody.fields.keySet.toList)
                  },
                  adts = lkg.adts,
                  funs = lkg.funs.map{
                    (s, fdef) => (s, FunSig(fdef.name, fdef.t))},
                  cases = lkg.cases.map{
                    (s, cdef) => (s, CasesSig(cdef.name, cdef.matchType, cdef.marker, cdef.t))},
                  nested = lkg.nested.map{
                    (s, nestlkg) => (s, convertDefToTyp(nestlkg))
                  })
  }

  def parseProgramTypLink(inp: String): TypingLinkage = {
    val deflink = parseProgramDefLink(inp)
    convertDefToTyp(deflink)
  }
}
