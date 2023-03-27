import scala.util.parsing.combinator.*
import scala.annotation.tailrec
import PersimmonSyntax.*


class PersimmonDefParser extends RegexParsers with PackratParsers {

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
  lazy val pDefaultRecField: PackratParser[(String, (Type, Option[Expression]))] =
    pFieldName ~ (":" ~> pType) ~ ("=" ~> pExp).? ^^ { case f~t~oe => f->(t->oe) }
  // separate parser for record type definition with defaults
  lazy val pDefaultRecType: PackratParser[(RecType, Rec)] = "{"~> repsep(pDefaultRecField, ",") <~"}" ^^ {
    lst =>
      if hasDuplicateName(lst) // disallow records with duplicate fields
      then throw new Exception("Parsing a record type with duplicate fields.")
      else {
        val type_fields = lst.collect{case (s, (t, _)) => (s, t)}.toMap;
        val defaults = lst.collect{case (s, (t, Some(e))) => (s, e)}.toMap;
        RecType(type_fields) -> Rec(defaults)
      }
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
        else AdtDefn(n, m, DefnBody(Some(cs.toMap), None, None))
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
  lazy val pTypeDef: PackratParser[(String, (Marker, (RecType, Rec)))] =
    kwType ~> pTypeName ~ pMarker ~ pDefaultRecType ^^ { case n~m~rt => n -> (m -> rt) }
  lazy val pAdtDef: PackratParser[(String, AdtDefn)] =
    pAdt ^^ { a => a.name -> a }

  lazy val pFunDef: PackratParser[(String, FunDefn)] =
    kwVal ~> pFunctionName ~ (":" ~> optBetween("(", ")", pFunType)) ~ ("=" ~> pExp) ^^ {
      case n~t~b => n -> FunDefn(n, t, DefnBody(Some(b), None, None))
    }

  lazy val pMatchType: PackratParser[PathType] = between("<", ">", pFamType)
  // mt = match type, m = marker, ft = funtype, lam = body
  lazy val pCasesDef: PackratParser[(String, CasesDefn)] =
    kwCases ~> pFunctionName ~ pMatchType ~ (":" ~> optBetween("(", ")", pFunType)) ~ pMarker ~ pExp ^^ {
      case n~mt~ft~m~b => n -> CasesDefn(n, mt, ft, m, DefnBody(Some(b), None, None))
    }


  // Replaces occurrences of any variable id in s with a projection x.id
  def var2proj(x: Expression, s: Set[String])(e: Expression): Expression = {
    val f = var2proj(x, s)
    e match {
      case Var(id) if s.contains(id) => Proj(x, id)
      case Lam(v, t, body) => Lam(v, t, f(body))
      case App(e1, e2) => App(f(e1), f(e2))
      case Rec(fields) => Rec(fields.mapValues(f).toMap)
      case Proj(e, name) => Proj(f(e), name)
      case Inst(t, rec) => Inst(t, f(rec).asInstanceOf[Rec])
      case InstADT(t, cname, rec) => InstADT(t, cname, f(rec).asInstanceOf[Rec])
      case Match(e, c, r) => Match(f(e), f(c).asInstanceOf[FamCases], f(r).asInstanceOf[Rec])
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
      val casesType = RecType(bodies.map{c => (c.constructor -> FunType(RecType(c.params.toMap), returnType))}.toMap)
      val inputType = RecType(params.toMap)
      val foldedType = params.foldRight(FunType(matchType, returnType)){
        case ((p,t),r) => FunType(t, r)}
      val t = FunType(inputType, casesType)
      val fun = marker match {
        case Eq => {
          val body0 = Lam(matched_var, matchType, Match(matched_var,
            FamCases(None, name_cases), Rec(params.map{(k,_) => (k -> Var("_"+k))}.toMap)))
          val paramsTr = params.map{(k,v) => ("_"+k, v)}.toMap
          val body = paramsTr.foldRight(body0){case ((p,t),r) =>
            Lam(Var(p), t, r)
          }
          Some(FunDefn(name, foldedType, DefnBody(Some(body), None, None)))
        }
        case PlusEq => None
      }
      val b = Lam(x, inputType, Rec(bodies.map{c => c.constructor ->
        var2proj(x, params.map(_._1).toSet)(
          Lam(matched_var, RecType(c.params.toMap),
            var2proj(matched_var, c.params.map(_._1).toSet)(
              c.body)))}.toMap))
      val cases = CasesDefn(name_cases, matchType, t, marker, DefnBody(Some(b), None, None))
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

  // A family can extend another family. If it does not, the parent is None.
  def pFamDef(selfPrefix: SelfPath): PackratParser[(String, DefinitionLinkage)] = {
    for {
      fam <- kwFamily ~> pFamilyName
      curSelfPath = SelfFamily(Sp(selfPrefix), fam)
      supFam <- (kwExtends ~> pAbsoluteFamPath).?
      typs~adts~funs0~extended~cases0~nested <- between("{", "}",
        rep(pTypeDef) ~ rep(pAdtDef) ~ rep(pFunDef) ~ rep(pExtendedDef) ~ rep(pCasesDef) ~ rep(pFamDef(curSelfPath))
      )
    } yield {
      val funs = funs0 ++ extended.filter{_._2._1.nonEmpty}.map{(k,v) => (k -> v._1.get)}
      val cases = cases0 ++ extended.map{(k,v) => (k+cases_suffix -> v._2)}

      if hasDuplicateName(typs) then throw new Exception("Parsing duplicate type names.")
      else if hasDuplicateName(adts) then throw new Exception("Parsing duplicate ADT names.")
      else if hasDuplicateName(funs) then throw new Exception("Parsing duplicate function names.")
      else if hasDuplicateName(cases) then throw new Exception("Parsing duplicate cases names.")
      else if hasDuplicateName(nested) then throw new Exception("Parsing duplicate family names.")
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
        val typedefs = typs.map { case (s, (m, (rt, r))) => s -> TypeDefn(s, m, DefnBody(Some(rt), None, None)) }.toMap
        val defaults = typs.collect{ case (s, (m, (rt, r))) => s -> DefaultDefn(s, m, DefnBody(Some(r), None, None)) }.toMap

        val funHeaders = funs.map { 
          case (s, fundefn) => s -> fundefn.t
        }.toMap
        val casesHeaders = cases.map { 
          case (s, casedefn) => s -> (casedefn.matchType, casedefn.t)
        }.toMap
        
        fam -> DefinitionLinkage(
          concretizePath(Sp(curSelfPath)),
          curSelfPath,
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

  lazy val pProgram: PackratParser[DefinitionLinkage] =
    rep(pFamDef(Prog)) ^^ {
      fams => 
        DefinitionLinkage(
          Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(),
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

object TestDefParser extends PersimmonDefParser {
  def parse0[T](p: PackratParser[T], inp: String): ParseResult[T] = parseAll(phrase(p), removeComments(inp))
  def canParse[T](p: PackratParser[T], inp: String): Boolean = parse0(p, inp).successful
  def parseSuccess[T](p: PackratParser[T], inp: String): T = parse0(p, inp).get
}
