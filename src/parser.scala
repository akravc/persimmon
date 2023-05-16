import scala.util.parsing.combinator.*
import scala.annotation.tailrec
import PersimmonSyntax.*

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
  lazy val pRecType: PackratParser[RecordType] = between("{", "}", repsep(pRecField, ",") ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record type with duplicate fields.")
      else RecordType(lst.toMap)
  })
  lazy val pFamType: PackratParser[PathType] =
    pPathExtra ^^ { case (p,t) => PathType(Some(p), t) }
    | pTypeName ^^ { t => PathType(None, t) } // TODO: where do we fill these in later on? "None" needs to become selfpath

  lazy val pNType: PackratParser[Type] = kwN ^^^ NType
  lazy val pBType: PackratParser[Type] = kwB ^^^ BType

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
    | pFamType | between("(", ")", pType)

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
  lazy val pExpPlus: PackratParser[Plus] = pExp ~ "+" ~ pExp ^^ { case e~_~g => Plus(e, g) }
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
    pExpProj | pExpMatch | pExpInstAdt | pExpInst | pExpApp | pExpPlus | pExpRec
    | pExpExtendedApp
    | pExpIfThenElse | pExpLam | pExpBool | pExpNat
    | pExpFamFun | pExpFamCases
    | pExpVar
    | between("(", ")", pExp)

  // MARKERS
  lazy val pMarker: PackratParser[Marker] =
    "=" ^^ {_ => Eq} | "+=" ^^ {_ => PlusEq}

  // DEFINITIONS
  lazy val pTypeDef: PackratParser[(String, (Marker, (RecordType, Record)))] =
    kwType ~> pTypeName ~ pMarker ~ pDefaultRecType ^^ { case n~m~rt => n -> (m -> rt) }
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
  type ExtendedDef = (Option[FunDefn], CasesDefn)
  case class ExtendedDefCase(constructor: String, params: List[(String, Type)], body: Expression)
  def extendedDef(name: String, params: List[(String, Type)], matchType: PathType, returnType: Type, marker: Marker, bodies: List[ExtendedDefCase]): PackratParser[(String, ExtendedDef)] = {
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
      success(name -> (fun, cases))
    }
  }
  def extendedDefCase(constructor: String, params: List[(String, Type)], body: Expression): PackratParser[ExtendedDefCase] = {
    if (hasDuplicateName(params))
      failure(s"duplicate names in $params")
    else success(ExtendedDefCase(constructor, params, body))
  }

  lazy val pExtendedDef: PackratParser[(String, ExtendedDef)] =
    kwDef ~> pFunctionName ~ (("(" ~> repsep(pRecField, ",") <~ ")") | success(Nil)) ~ (":" ~> optBetween("(", ")", (pFamType ~ ("->" ~> pType)))) ~ pMarker >> {
      case n~p~(f~t)~m => repsep(pExtendedDefCase, "") >> {bs =>
        extendedDef(n, p, f, t, m, bs)
      }
    }

  lazy val pExtendedDefCase: PackratParser[ExtendedDefCase] =
    (kwCase ~> pConstructorName ~ ("(" ~> repsep(pRecField, ",") <~ ")" <~ "=") ~ pExp >> {
      case c~p~e => extendedDefCase(c, p, e)
    }) | (kwCase ~> "_" ~> "=" ~> pExp >> {e => extendedDefCase("_", Nil, e)})
  
  def pFamBody(curSelfPath: SelfPath): PackratParser[DefinitionLinkage] = {
    for {
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
        supFam match {
          case Some(b) =>
            /* TODO: Do this in the extend cycle check in later phase.
            if (a == b) then
              throw new Exception("Parsing a family that extends itself.")
            else
             */
            // family extends another
            if typs.exists{case (s, (m, (rt, r))) => (m == PlusEq) && (rt.fields.keySet != r.fields.keySet)} then
              throw new Exception("In a type extension, not all fields have defaults.");
            else ()
          // family does not extend another
          case None => ()
        }
        val typedefs = typs.map { case (s, (m, (rt, r))) => s -> TypeDefn(s, m, rt) }.toMap
        val defaults = typs.collect{ case (s, (m, (rt, r))) => s -> DefaultDefn(s, m, r) }.toMap

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
          new_nested.toMap
        )
      }
    }
  }

  // A family can extend another family. If it does not, the parent is None.
  def pFamDef(selfPrefix: SelfPath): PackratParser[(String, DefinitionLinkage)] = {
    for {
      fam <- kwFamily ~> pFamilyName
      curSelfPath = SelfFamily(Sp(selfPrefix), fam)
      linkage <- pFamBody(curSelfPath)
    } yield {
      fam -> linkage
    }
  }
  
  def pMixDef(selfPrefix: SelfPath): PackratParser[(String, DefinitionLinkage)] = {
    for {
      mix <- kwMixin ~> pFamilyName
      curSelfPath = SelfFamily(Sp(selfPrefix), mix)
      baseSelfPath = SelfFamily(Sp(curSelfPath), "#Base")
      linkage <- pFamBody(baseSelfPath)
    } yield {
      mix -> linkage
    }
  }

  lazy val pProgram: PackratParser[DefinitionLinkage] =
    (rep(pMixDef(Prog)) ~ rep(pFamDef(Prog))) ^^ { case mixins ~ fams =>
      val new_fams = mixins ++ fams
      if hasDuplicateName(new_fams) then throw new Exception("Parsing duplicate family names.")
      DefinitionLinkage(Sp(Prog), None, Map(), Map(), Map(), Map(), Map(), new_fams.toMap)
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

  def fillNonePaths(lkg: DefinitionLinkage): DefinitionLinkage = {
    var selfpath = lkg.self
    DefinitionLinkage(
      lkg.self, lkg.sup, 
      lkg.types.map((s, tdef) => 
        (s, TypeDefn(tdef.name, tdef.marker, fillNonePathsInType(tdef.typeBody, selfpath).asInstanceOf[RecordType]))),
      lkg.defaults.map((s, ddef) => 
        (s, DefaultDefn(ddef.name, ddef.marker, fillNonePathsInExp(ddef.defaultBody, selfpath).asInstanceOf[Record]))),
      lkg.adts.map((s, adef) => 
        (s, AdtDefn(adef.name, adef.marker, 
          adef.adtBody.map((c, rt) => 
            (c, fillNonePathsInType(rt, selfpath).asInstanceOf[RecordType]))))),
      lkg.funs.map((s, fd) => 
        (s, FunDefn(fd.name, fillNonePathsInType(fd.t, selfpath).asInstanceOf[FunType], fillNonePathsInExp(fd.funBody, selfpath).asInstanceOf[Lam]))),
      lkg.cases.map((s, cd) => (s, CasesDefn(cd.name, 
      fillNonePathsInType(cd.matchType, selfpath).asInstanceOf[PathType], 
      fillNonePathsInType(cd.t, selfpath).asInstanceOf[FunType], 
      cd.ts.map(x => fillNonePathsInType(x, selfpath)), cd.marker, 
      fillNonePathsInExp(cd.casesBody, selfpath)))),
      lkg.nested.map((s, l) => (s, fillNonePaths(l)))
    )
  }

  def fillNonePathsInType(t: Type, p: Path): Type = {
    t match {
      case FunType(input, output) => 
        FunType(fillNonePathsInType(input, p), fillNonePathsInType(output, p))
      case PathType(path, name) => 
        if (path == None) then PathType(Some(p), name) else t
      case RecordType(fields) => 
        RecordType(fields.map{(s, t) => (s, fillNonePathsInType(t, p))})
      case _ => t
    }
  }

  def fillNonePathsInExp(e: Expression, p: Path): Expression = {
    e match {
      case Lam(v, t, body) => 
        Lam(v, fillNonePathsInType(t, p), fillNonePathsInExp(body, p))
      case FamFun(path, name) =>
        if (path == None) then FamFun(Some(p), name) else e
      case FamCases(path, name) =>
        if (path == None) then FamCases(Some(p), name) else e
      case App(e1, e2) => App(fillNonePathsInExp(e1, p), fillNonePathsInExp(e2, p))
      case Plus(e1, e2) => Plus(fillNonePathsInExp(e1, p), fillNonePathsInExp(e2, p))
      case Record(fields) => 
        Record(fields.map{(s, d) => (s, fillNonePathsInExp(d, p))})
      case Proj(r, name) => Proj(fillNonePathsInExp(r, p), name)
      case Inst(t, rec) => Inst(fillNonePathsInType(t, p).asInstanceOf[PathType], fillNonePathsInExp(rec, p).asInstanceOf[Record])
      case InstADT(t, cname, rec) => InstADT(fillNonePathsInType(t, p).asInstanceOf[PathType], cname, fillNonePathsInExp(rec, p).asInstanceOf[Record])
      case Match(m, c, r) => Match(fillNonePathsInExp(m, p), fillNonePathsInExp(c, p).asInstanceOf[FamCases], fillNonePathsInExp(r, p).asInstanceOf[Record])
      case IfThenElse(condExpr, ifExpr, elseExpr) => 
        IfThenElse(fillNonePathsInExp(condExpr, p), fillNonePathsInExp(ifExpr, p), fillNonePathsInExp(elseExpr, p))
      case _ => e
    }
  }

  def parseProgramDefLink(inp: String): DefinitionLinkage = {
    fillNonePaths(parse0(pProgram, inp).get)
  }

  def convertDefToTyp(lkg: DefinitionLinkage): TypingLinkage = {
    TypingLinkage(self = lkg.self, 
                  sup = lkg.sup,
                  types = lkg.types,
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
    var deflink = parseProgramDefLink(inp)
    convertDefToTyp(deflink)
  }
}
