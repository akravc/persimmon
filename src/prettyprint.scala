import PersimmonSyntax._

object PrettyPrint {

  def printSP(sp: SelfPath): String = sp match {
    case Prog => "<>"
    case SelfFamily(p, f) => "self(" + printPath(p) + "." + f + ")"
  }

  def printPath(p: Path) : String = {
    p match {
      case Sp(sp) => printSP(sp)
      case AbsoluteFamily(p, f) => printPath(p) + "." + f
    }
  }

  def printType(t: Type): String = {
    t match {
      case NType => "N"
      case BType => "B"
      case FunType(a, b) => "(" + printType(a) + " -> " + printType(b) + ")"
      case PathType(path, n) => path.map(printPath).getOrElse("None") + "." + n
      case RecType(fields) =>
        val printmap = fields.map{case (f, t) =>
          if fields.last == (f, t) then f + ": " + printType(t)
          else f + ": " + printType(t) + ", "}
        "{" + printmap.mkString + "}"
    }
  }

  def printMarker(m: Marker): String = {
    m match {
      case Eq => " = "
      case PlusEq => " += "
    }
  }
  
  def printExp(e: Expression) : String = {
    e match {
      case NExp(n) => ""+ n
      case BExp(b) => ""+ b
      case Var(id) => id
      case Lam(v, t, body) => "lam (" + printExp(v) + ": " + printType(t) + "). " + printExp(body)
      case FamFun(p, n) => p.map(printPath).getOrElse("None") + "." + n
      case FamCases(p, n) => "<" + p.map(printPath).getOrElse("None") + "." + n + ">"
      case App(e, g) => "(" + printExp(e) + " " + printExp(g) + ")"
      case Rec(fields) =>
        val printmap = fields.map{case (f, e) =>
          if ((f, e) == fields.last) then f + " = " + printExp(e)
          else f + " = " + printExp(e) + ", "}
        "{"+ printmap.mkString + "}"
      case Proj(e, n) => printExp(e) + "." + n
      case Inst(t, r) => printType(t) + " (" + printExp(r) + ")"
      case InstADT(t, c, r) => printType(t) + " (" + c + " " + printExp(r) + ")"
      case Match(e, fc, r) => "match " + printExp(e) + " with " + printExp(App(fc, r))
      case IfThenElse(condExpr, ifExpr, elseExpr) =>
        s"if ${printExp(condExpr)} then ${printExp(ifExpr)} else ${printExp(elseExpr)}"
    }
  }

  def printADT(a: AdtDefn) : String = a match {
    case AdtDefn(name, marker, adtBody) =>
      "type " + name + printMarker(marker) +
        printBody(adtBody)(_.map {
          (c, r) => c + " " + printType(r)
        }.mkString(" | ")) +
        "\n"
  }

  def printBody[B](body: DefnBody[B])(printB: B => String): String = {
    val DefnBody(defn, extendsFrom, furtherBindsFrom, _) = body
    val bPretty = defn.map(printB)
    s"[$bPretty, extends from: ${extendsFrom.map(printPath)}, further binds from: ${furtherBindsFrom.map(printPath)}]"
  }

  def printLkg(lkg: Linkage, offset: String): Unit = {
    print(offset + "LINKAGE DEFINITION: \n\n")

    print(offset + "PATH: " + printPath(lkg.getPath()) + "\n\n")

    print(offset + "SELF: " + printSP(lkg.getSelfPath()) + "\n\n")

    print(offset + "SUPER: " + lkg.getSuperPath().map(printPath).getOrElse("None") + "\n\n")

    print(offset + "NESTED:\n")
    lkg.getAllNested().map {
        case (s, lkg) => print(("\t" + offset) + s + " -> {\n"); printLkg(lkg, ("\t" + offset)); print(offset + "}\n")
    }
    print("\n\n")

    print(offset + "TYPES:\n")
    val typemap = lkg.getTypes().view.mapValues{
      case TypeDefn(name, marker, typeBody) => "type " + name + printMarker(marker) +  printBody(typeBody)(printType) + "\n"
    }
    print(offset + typemap.mkString)
    print("\n\n")

    
    lkg.getDefaults() match {
        case None => print("")
        case Some(map) =>
            print(offset + "DEFAULTS:\n")
            val defmap = map.view.mapValues {
                case DefaultDefn(s, m, defaultBody) => "type " + s + printMarker(m) +  printBody(defaultBody)(printExp) + "\n"
            }
            print(offset + defmap.mkString)
            print("\n\n")
    }

    print(offset + "ADTs:\n")
    val adtmap = lkg.getAdts().map{
      case (s, adt) => printADT(adt) + "\n"
    }
    print(offset + adtmap.mkString)
    print("\n\n")

    print(offset + "FUNS:\n")
    var funmap: Iterable[String] = null
    lkg.getFuns() match {
        case Right(mdefs) =>
            funmap = mdefs.map{ case (_, FunDefn(s, ft, body)) =>
                "val " + s + ": " + printType(ft) + " = " + printBody(body)(printExp) + "\n"}
        case Left(msigs) => 
            funmap = msigs.map{ case (_, FunSig(s, ft)) =>
                "val " + s + ": " + printType(ft) + "\n" }
    }
    print(offset + funmap.mkString)
    print("\n\n")

    print(offset + "CASES:\n")
    var casemap: Iterable[String] = null
    lkg.getCases() match {
        case Right(cdefs) =>
            casemap = cdefs.map{ case (_, CasesDefn(s, mt, ft, _, m, body)) =>
                "cases " + s + "<" + printType(mt) + ">" + ": " + printType(ft) + printMarker(m) + printBody(body)(printExp) + "\n"}
        case Left(csigs) => 
            casemap = csigs.map{ case (_, CasesSig(s, mt, m, ft)) =>
                "cases " + s + "<" + printType(mt) + ">" + ": " + printType(ft) + printMarker(m) + "\n"}
            }
    print(offset + casemap.mkString)
    print("\n\n")
  }
}