import PersimmonSyntax.*
import PersimmonLinkages.*
import PersimmonWF.*
import PrettyPrint.*

object PersimmonTyping {
  def getType(K: PathCtx, Gamma: TypingCtx, e: Expression): Option[Type] = debugType(s"${printExp(e)} ($e)", e match {
    case NExp(n) => Some(NType)
    case BExp(b) => Some(BType)
    case Var(id) => Gamma.get(id)
    case Lam(v, t, b) =>
      if wfType(K, t) then
        getType(K, Gamma + (v.id -> t), b).map { bt => FunType(t, bt) }
      else None
    case FamFun(path, name) =>
      if path == None then None else {
        if !wfPath(K, path.get) then None else
        computeTypLinkage(path.get).funs.get(name).map { sig => sig.t }
      }
    case FamCases(path, name) =>
      if path == None then None else {
        if !wfPath(K, path.get) then None else
        computeTypLinkage(path.get).cases.get(name).map { sig => sig.t }
      }
    case App(e1, e2) => getType(K, Gamma, e1) match {
      case Some(t1: FunType) =>
        getType(K, Gamma, e2).flatMap { t2 =>
          if t1.input == t2 then Some(t1.output) else None
        }
      case _ => None
    }
    case Plus(e1, e2) => getType(K, Gamma, e1) match {
      case Some(NType) => getType(K, Gamma, e2) match {
          case Some(NType) => Some(NType)
          case _ => None }
      case _ => None
    }
    case Record(fields) => 
      val types = fields.mapValues { field => getType(K, Gamma, field) }.toMap
      if types.exists((_, t) => t.isEmpty) then None
      else Some(RecordType(types.mapValues { t => t.get }.toMap))
    case Proj(e, name) => getType(K, Gamma, e).flatMap { t =>
      getFieldType(K, t, name)
    }
    case Inst(t, rec) =>
      if !wfPath(K, t.path.get) then None else {
        val lkg = computeTypLinkage(t.path.get)
        lkg.types.get(t.name).flatMap { typeDefn =>
          val fields = typeDefn.typeBody.fields
          val defaults = lkg.defaults
          if fields.forall(
            (name, ft) => (defaults.get(t.name) match {
              case Some(deflist) => deflist.contains(name)
              case None => false
            }) || hasType(K, Gamma, rec.fields.get(name).get, ft)
          ) then Some(t) else None
        }
      }
    case InstADT(t, cname, rec) =>
      if !wfPath(K, t.path.get) then None else {
        computeTypLinkage(t.path.get).adts.get(t.name).flatMap { adtDefn =>
          adtDefn.adtBody.get(cname).flatMap { recType =>
            val fields = recType.fields
            if fields.keySet == rec.fields.keySet &&
            fields.forall { (name, ft) =>
              val fe = rec.fields.get(name).get
              debug(s"${printExp(e)}:${printType(ft)}", hasType(K, Gamma, fe, ft))
            } then Some(t) else None
          }
        }
      }
    case Match(e, c, r) =>
      getType(K, Gamma, e) match {
        case Some(t: PathType) =>
          val matchTypeLkg = computeTypLinkage(t.path.get)
          val adtDefinition = matchTypeLkg.adts.get(t.name)
          val argsType = getType(K, Gamma, r)
          val cType = getType(K, Gamma, c)

          if (adtDefinition == None) {
            println("adt definition missing")
            println("t: "+t)
            println("name: "+t.name)
            println("adts: "+matchTypeLkg.adts)
            println("c.path.get: "+c.path.get)
            println("matchTypeLkg: "+matchTypeLkg)
          }

          if (adtDefinition == None || argsType == None || cType == None) 
          then debugType("none", None)
          else {
            val cOutputType = cType.get.asInstanceOf[FunType].output.asInstanceOf[RecordType].fields
            val cInputType = cType.get.asInstanceOf[FunType].input
            val T = cOutputType.head._2.asInstanceOf[FunType].output

            if (debug("input type match", (cInputType == argsType.get)) &&
              debug("body match", adtDefinition.get.adtBody.forall {
              (constructorName, arguments) =>
                val handlerType = cOutputType.get(constructorName)
                if (handlerType == None) then false else {
                  arguments == handlerType.get.asInstanceOf[FunType].input && 
                  handlerType.get.asInstanceOf[FunType].output == T
                }
            })) then Some(T) else None
        }
        case t => debugType("not a path type: ${printType(t)}", None)
      }
    case IfThenElse(condExpr, ifExpr, elseExpr) =>
      if hasType(K, Gamma, condExpr, BType) then {
        getType(K, Gamma, ifExpr).flatMap { ifType =>
          getType(K, Gamma, elseExpr).flatMap { elseType =>
            if ifType == elseType then Some(ifType) else None
          }
        }
      } else None
  })
  
  def hasType(K: PathCtx, Gamma: TypingCtx, e: Expression, t: Type): Boolean = {
    getType(K, Gamma, e).exists { et =>
      isSubtype(K, et, t)
    }
  }

  def getFieldType(K: PathCtx, t: Type, fieldName: String): Option[Type] = t match {
    case PathType(path, name) =>
        if !wfPath(K, path.get) then None else {
          computeTypLinkage(path.get).types.get(name).flatMap { typeDefn =>
            getFieldType(K, typeDefn.typeBody, fieldName)
          }
        }
    case RecordType(fields) => fields.get(fieldName)
    case _ => None
  }

  def isSubtype(K: PathCtx, t1: Type, t2: Type): Boolean = {
    if t1 == t2 then true
    else t1 match {
      case PathType(path, name) =>
        if !wfPath(K, path.get) then false else {
          computeTypLinkage(path.get).types.get(name) match {
            case None => false
            case Some(typeDefn) => isSubtype(K, typeDefn.typeBody, t2)
          }
        }
      case FunType(input1, output1) =>
        t2 match {
          case FunType(input2, output2) =>
            isSubtype(K, input2, input1) && isSubtype(K, output1, output2)
          case _ => false
        }
      case RecordType(fields1) =>
        t2 match {
          case RecordType(fields2) =>
            fields2.forall {(name2, ft2) =>
              fields1.get(name2).exists { ft1 =>
                isSubtype(K, ft1, ft2)
            }
          }
          case _ => false
        }
      case _ => false
    }
  }

  def debugType(msg: => String, o: Option[Type]): Option[Type] = {
    if (o.isEmpty) {
      println("bad type at "+msg)
    }
    o
  }
}
