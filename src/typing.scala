import PersimmonSyntax.*
import PersimmonLinkages.*
import PersimmonWF.*
import PrettyPrint.*

object PersimmonTyping {
  def getType(K: PathCtx, Gamma: TypingCtx, e: Expression): Option[Type] = e match {
    case NExp(n) => Some(NType)
    case BExp(b) => Some(BType)
    case Var(id) => Gamma.get(id)
    case Lam(v, t, b) =>
      if wfType(K, t) then
        getType(K, Gamma + (v.id -> t), b).map { bt => FunType(t, bt) }
      else None
    case FamFun(path, name) =>
      if path == None then None else {
        computeTypLinkage(path.get).funs.get(name).map { sig => sig.t }
      }
    case FamCases(path, name) =>
      if path == None then None else {
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
    case Proj(e, name) => getType(K, Gamma, e).flatMap {
      case RecordType(fields) => fields.get(name)
      case _ => None
    }
    case Inst(t, rec) =>
      computeTypLinkage(t.path.get).types.get(t.name).flatMap { typeDefn =>
        val fields = typeDefn.typeBody.fields
        if fields.keySet == rec.fields.keySet && fields.forall(
          (name, ft) => hasType(K, Gamma, rec.fields.get(name).get, ft)
        ) then Some(t) else None
      }
    case InstADT(t, cname, rec) =>
      computeTypLinkage(t.path.get).adts.get(t.name).flatMap { adtDefn =>
        adtDefn.adtBody.get(cname).flatMap { recType =>
          val fields = recType.fields
          if fields.keySet == rec.fields.keySet &&
          fields.forall { (name, ft) =>
            hasType(K, Gamma, rec.fields.get(name).get, ft)
          } then Some(t) else None
        }
      }
    case Match(e, c, r) =>
      getType(K, Gamma, e) match {
        case Some(t: PathType) =>
          val matchTypeLkg = computeTypLinkage(c.path.get)
          val adtDefinition = matchTypeLkg.adts.get(t.name)
          val argsType = getType(K, Gamma, r)
          val cType = getType(K, Gamma, c)

          if (adtDefinition == None || argsType == None || cType == None) 
          then None
          else {
            val cOutputType = cType.get.asInstanceOf[FunType].output.asInstanceOf[RecordType].fields
            val cInputType = cType.get.asInstanceOf[FunType].input
            val T = cOutputType.head._2.asInstanceOf[FunType].output

            if ((cInputType == argsType.get) && adtDefinition.get.adtBody.forall {
              (constructorName, arguments) =>
                val handlerType = cOutputType.get(constructorName)
                if (handlerType == None) then false else {
                  arguments == handlerType.get.asInstanceOf[FunType].input && 
                  handlerType.get.asInstanceOf[FunType].output == T
                }
            }) then Some(T) else None
        }
        case _ => None
      }
    case IfThenElse(condExpr, ifExpr, elseExpr) =>
      if getType(K, Gamma, condExpr) == BType then {
        getType(K, Gamma, ifExpr).flatMap { ifType =>
          getType(K, Gamma, elseExpr).flatMap { elseType =>
            if ifType == elseType then Some(ifType) else None
          }
        }
      } else None
  }
  
  def hasType(K: PathCtx, Gamma: TypingCtx, e: Expression, t: Type): Boolean = {
    getType(K, Gamma, e).exists { et =>
      isSubtype(K, et, t)
    }
  }
  
  def isSubtype(K: PathCtx, t1: Type, t2: Type): Boolean = {
    if t1 == t2 then true
    else t1 match {
      case PathType(path, name) =>
        val typeDefn = computeTypLinkage(path.get).types.get(name).get
        isSubtype(K, typeDefn.typeBody, t2)
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
}
