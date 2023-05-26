import PersimmonSyntax.*
import PersimmonLinkages.*
import PersimmonWF.*
import PrettyPrint.*
import MapOps.*

object PersimmonTyping {
  def getType(K: PathCtx, Gamma: TypingCtx, e: Expression): Either[String,Type] = e match {
    case NExp(n) => Right(NType)
    case BExp(b) => Right(BType)
    case Var(id) => Gamma.get(id).fold(Left(s"Variable $id is unbound"))(Right.apply)
    case Lam(v, t, b) =>
      if wfType(K, t) then
        getType(K, Gamma + (v.id -> t), b).map { bt => FunType(t, bt) }
      else Left(s"Type ${printType(t)} in expression ${printExp(e)} is not well-formed")
    case FamFun(path, name) =>
      if path == None then Left(s"Expression ${printExp(e)} is missing a path") else {
        if !wfPath(K, path.get) then Left(s"Path ${printPath(path.get)} in expression ${printExp(e)} is not well-formed")
        else computeTypLinkage(path.get).funs.get(name) match {
          case Some(sig) => Right(sig.t)
          case None => Left(s"Linkage missing $name in expression ${printExp(e)}")
        }
      }
    case FamCases(path, name) =>
      if path == None then Left(s"Expression ${printExp(e)} is missing a path") else {
        if !wfPath(K, path.get) then Left(s"Path ${printPath(path.get)} in expression ${printExp(e)} is not well-formed")
        else computeTypLinkage(path.get).cases.get(name) match {
          case Some(sig) => Right(sig.t)
          case None => Left(s"Linkage missing $name in expression ${printExp(e)}")
        }
      }
    case App(e1, e2) => getType(K, Gamma, e1) match {
      case Right(t1: FunType) =>
        getType(K, Gamma, e2).flatMap { t2 =>
          if t1.input == t2 then Right(t1.output)
          else Left(s"Input types ${printType(t1.input)} and ${printType(t2)} do not match in expression ${printExp(e)}")
        }
      case Right(t1) => Left(s"Type ${printType(t1)} is not a function type in expression ${printExp(e)}")
      case Left(msg) => Left(msg)
    }
    case Plus(e1, e2) => getType(K, Gamma, e1) match {
      case Right(NType) => getType(K, Gamma, e2) match {
        case Right(NType) => Right(NType)
        case Right(t2) => Left(s"Expected type N, got ${printType(t2)} for expression ${printExp(e2)} in expression ${printExp(e)}")
        case Left(msg) => Left(msg)
      }
      case Right(t1) => Left(s"Expected type N, got ${printType(t1)} for expression ${printExp(e1)} in expression ${printExp(e)}")
      case Left(msg) => Left(msg)
    }
    case Record(fields) => 
      traverseMap(fields){field => getType(K, Gamma, field)}.map(RecordType.apply)
    case Proj(e, name) => getType(K, Gamma, e).flatMap { t =>
      getFieldType(K, t, name)
    }
    case Inst(t, rec) =>
      if !wfPath(K, t.path.get) then Left(s"Path ${printPath(t.path.get)} in expression ${printExp(e)} is not well-formed") else {
        val lkg = computeTypLinkage(t.path.get)
        lkg.types.get(t.name) match {
          case Some(typeDefn) => {
          val fields = typeDefn.typeBody.fields
          val defaults = lkg.defaults
          if fields.forall(
            (name, ft) => (defaults.get(t.name) match {
              case Some(deflist) => deflist.contains(name)
              case None => false
            }) || hasType(K, Gamma, rec.fields.get(name).get, ft)
          ) then Right(t) else Left(s"Ill-typed expression ${printExp(e)}")
          }
          case None => Left(s"Missing record type ${t.name} in linkage for expression ${printExp(e)}")
        }
      }
    case InstADT(t, cname, rec) =>
      if !wfPath(K, t.path.get) then Left(s"Path ${printPath(t.path.get)} in expression ${printExp(e)} is not well-formed") else {
        computeTypLinkage(t.path.get).adts.get(t.name) match {
          case Some(adtDefn) => {
            adtDefn.adtBody.get(cname) match {
              case Some(recType) => {
                val fields = recType.fields
                if fields.keySet == rec.fields.keySet &&
                fields.forall { (name, ft) =>
                  hasType(K, Gamma, rec.fields.get(name).get, ft)
                } then Right(t) else Left(s"Ill-typed expression ${printExp(e)}")
              }
              case None => Left(s"Missing ADT constructor $cname in linkage for expression ${printExp(e)}")
            }
          }
          case None => Left(s"Missing ADT type ${t.name} in linkage for expression ${printExp(e)}")
        }
      }
    case m@Match(e, c, r) =>
      getType(K, Gamma, e) match {
        case Right(t: PathType) =>
          val matchTypeLkg = computeTypLinkage(t.path.get)
          val adtDefinition = matchTypeLkg.adts.get(t.name)
          val argsType = getType(K, Gamma, r)
          val cType = getType(K, Gamma, c)

          if adtDefinition.isEmpty
          then Left(s"Missing ADT ${t.name} in linkage for expression ${printExp(m)}")
          else if argsType.isLeft
          then argsType
          else if cType.isLeft
          then cType
          else {
            val cOutputType = right(cType).asInstanceOf[FunType].output.asInstanceOf[RecordType].fields
            val cInputType = right(cType).asInstanceOf[FunType].input
            val T = cOutputType.head._2.asInstanceOf[FunType].output

            if ((cInputType == right(argsType)) && adtDefinition.get.adtBody.forall {
              (constructorName, arguments) =>
                val handlerType = cOutputType.get(constructorName)
                if (handlerType == None) then false else {
                  arguments == handlerType.get.asInstanceOf[FunType].input && 
                  handlerType.get.asInstanceOf[FunType].output == T
                }
            }) then Right(T) else Left(s"Ill-typed match expression ${printExp(m)}")
          }
        case Right(t) => Left(s"Expected a path type, not ${printType(t)}, for expression ${printExp(e)} in expression ${printExp(m)}")
        case Left(msg) => Left(msg)
      }
    case IfThenElse(condExpr, ifExpr, elseExpr) =>
      if hasType(K, Gamma, condExpr, BType) then {
        getType(K, Gamma, ifExpr).flatMap { ifType =>
          getType(K, Gamma, elseExpr).flatMap { elseType =>
            if ifType == elseType then Right(ifType)
            else Left(s"Types of if branches do not match: ${printType(ifType)} vs ${printType(elseType)} in expression ${printExp(e)}")
          }
        }
      } else Left(s"Expected condition ${printExp(condExpr)} to have type B in expression ${printExp(e)}")
  }
  
  def hasType(K: PathCtx, Gamma: TypingCtx, e: Expression, t: Type): Boolean = {
    getType(K, Gamma, e).exists { et =>
      isSubtype(K, et, t)
    }
  }

  def getFieldType(K: PathCtx, t: Type, fieldName: String): Either[String,Type] = t match {
    case PathType(path, name) =>
        if !wfPath(K, path.get) then Left(s"Path ${printPath(path.get)} is not well-formed") else {
          computeTypLinkage(path.get).types.get(name) match {
            case Some(typeDefn) => getFieldType(K, typeDefn.typeBody, fieldName)
            case None => Left(s"Missing record type $name in linkage for type ${printType(t)}")
          }
        }
    case RecordType(fields) => fields.get(fieldName).fold(Left(s"Missing field $fieldName"))(Right.apply)
    case _ => Left(s"Cannot project from type ${printType(t)}")
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
}
