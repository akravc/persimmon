import PersimmonSyntax.*
import PersimmonLinkages.*

object PersimmonTyping {
  def isWellFormed(K: PathCtx, t: Type): Boolean = t match {
    case NType => true
    case BType => true
    case FunType(input, output) => isWellFormed(K, input) && isWellFormed(K, output)
    case PathType(path, name) =>
      val linkage = computeTypLinkage(K, path.get)
      linkage.types.contains(name) || linkage.adts.contains(name)
    case RecType(fields) =>
      fields.forall { (name, t) => isWellFormed(K, t) }
  }
  
  def getType(K: PathCtx, Gamma: TypingCtx, e: Expression): Option[Type] = e match {
    case NExp(n) => Some(NType)
    case BExp(b) => Some(BType)
    case Var(id) => Gamma.get(id)
    case Lam(v, t, b) =>
      if isWellFormed(K, t) then
        getType(K, Gamma + (v.id -> t), b).map { bt => FunType(t, bt) }
      else None
    case FamFun(path, name) =>
      computeTypLinkage(K, path.get).funs.get(name).map { sig => sig.t }
    case FamCases(path, name) =>
      computeTypLinkage(K, path.get).cases.get(name).map { sig => sig.t }
    case App(e1, e2) => getType(K, Gamma, e1) match {
      case Some(t1: FunType) =>
        getType(K, Gamma, e2).flatMap { t2 =>
          if t1.input == t2 then Some(t1.output) else None
        }
      case _ => None
    }
    case Rec(fields) =>
      val types = fields.mapValues { field => getType(K, Gamma, field) }.toMap
      if types.exists((_, t) => t.isEmpty) then None
      else Some(RecType(types.mapValues { t => t.get }.toMap))
    case Proj(e, name) => getType(K, Gamma, e).flatMap {
      case RecType(fields) => fields.get(name)
      case _ => None
    }
    case Inst(t, rec) =>
      computeTypLinkage(K, t.path.get).types.get(t.name).flatMap { typeDefn =>
        val fields = typeDefn.typeBody.defn.get.fields
        if fields.keySet == rec.fields.keySet && fields.forall(
          (name, ft) => getType(K, Gamma, rec.fields.get(name).get) == Some(ft)
        ) then Some(t) else None
      }
    case InstADT(t, cname, rec) =>
      computeTypLinkage(K, t.path.get).adts.get(t.name).flatMap { adtDefn =>
        val cases = adtDefn.adtBody.defn.get
        cases.get(cname).flatMap { recType =>
          val fields = rec.fields
          if fields.keySet == rec.fields.keySet &&
          fields.forall { (name, ft) =>
            getType(K, Gamma, rec.fields.get(name).get) == Some(ft)
          } then Some(t) else None
        }
      }
    case Match(e, c, r) =>
      getType(K, Gamma, e) match {
        case Some(t: PathType) =>
          computeTypLinkage(K, c.path.get).cases.get(c.name).flatMap { sig => 
            val funType = sig.t
            if sig.mt == t && getType(K, Gamma, r) == Some(funType.input)
            then Some(funType.output) else None
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
        val typeDefn = computeTypLinkage(K, path.get).types.get(name).get
        isSubtype(K, typeDefn.typeBody.defn.get, t2)
      case FunType(input1, output1) =>
        t2 match {
          case FunType(input2, output2) =>
            isSubtype(K, input2, input1) && isSubtype(K, output1, output2)
          case _ => false
        }
      case RecType(fields1) =>
        t2 match {
          case RecType(fields2) =>
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


// OLD VERSION

// import PersimmonSyntax.*
// import PersimmonLinkages.*

// object PersimmonTyping {
    
//     def getType(K: PathCtx, Gamma: TypingCtx, e: Expression): Option[Type] = e match {
//         case NExp(n) => Some(NType)
//         case BExp(b) => Some(BType)
//         case Var(id) => Gamma.get(id)
//         case Lam(v, t, b) =>
//             getType(K, Gamma + (v.id -> t), b).map {
//                 bt => FunType(t, bt)
//             }
//         case FamFun(path, name) =>
//             computeTypLinkage(K, path.get).funs.get(name) match {
//                 case Some(sig) => Some(sig.t)
//                 case _ => None
//             }
//         case FamCases(path, name) =>
//             computeTypLinkage(K, path.get).cases.get(name) match {
//                 case Some(sig) => Some(sig.t)
//                 case _ => None
//             }
//         case App(e1, e2) => {
//             val t1 = getType(K, Gamma, e1) match {
//                 case Some(t: FunType) => t
//                 case _ => return None
//             }
//             val t2 = getType(K, Gamma, e2) match {
//                 case Some(t) => t
//                 case _ => return None
//             }
//             if t1.input == t2 then Some(t1.output) else None
//         }
//         case Rec(fields) => {
//             val types = fields.mapValues {
//                 field => getType(K, Gamma, field)
//             }.toMap
//             if types.exists((_, t) => t.isEmpty) then None
//             Some(RecType(types.mapValues(t => t.get).toMap))
//         }
//         case Proj(e, name) => getType(K, Gamma, e).flatMap {
//             case RecType(fields) => fields.get(name)
//             case _ => None
//         }
//         case Inst(t, rec) => {
//             val fields = computeTypLinkage(K, t.path.get).types.get(t.name) match {
//                 case Some(typeDefn) => typeDefn.typeBody.defn.get.fields
//                 case _ => return None
//             }
//             if fields.keySet != rec.fields.keySet then return None
//             if fields.forall(
//                 (name, t) => getType(K, Gamma, rec.fields.get(name).get) == Some(t)
//             ) then Some(t) else None
//         }
//         // TODO: Do some extra checks.
//         case InstADT(t, cname, rec) =>
//             val cases = computeTypLinkage(K, t.path.get).adts.get(t.name) match {
//                 case Some(adtDefn) => adtDefn.adtBody.defn.get
//                 case _ => return None
//             }
//             val fields = cases.get(cname) match {
//                 case Some(rec) => rec.fields
//                 case _ => return None
//             }
//             if fields.keySet != rec.fields.keySet then return None
//             if fields.forall(
//                 (name, t) => getType(K, Gamma, rec.fields.get(name).get) == Some(t)
//             ) then Some(t) else None
//         case Match(e, c, r) =>
//             val t = getType(K, Gamma, e) match {
//                 case Some(t: PathType) => t
//                 case _ => return None
//             }
//             val funType = computeTypLinkage(K, c.path.get).cases.get(c.name) match {
//                 case Some(sig) => 
//                     if sig.mt != t then return None
//                     sig.t
//                 case _ => return None
//             }
//             if getType(K, Gamma, r) != Some(funType.input) then return None
//             Some(funType.output)
//         case IfThenElse(condExpr, ifExpr, elseExpr) =>
//             if getType(K, Gamma, condExpr) != BType then return None
//             val ifType = getType(K, Gamma, ifExpr) match {
//                 case Some(t) => Some(t)
//                 case _ => return None
//             }
//             val elseType = getType(K, Gamma, elseExpr) match {
//                 case Some(t) => Some(t)
//                 case _ => return None
//             }
//             if ifType == elseType then ifType else None
//     }
// }
