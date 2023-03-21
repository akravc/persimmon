import PersimmonSyntax.*
import PersimmonLinkages.*

object PersimmonTyping {
    type TypingCtx = Map[String, Type] // Gamma
    type PathCtx = List[Path] // K
    
    def getType(K: PathCtx, Gamma: TypingCtx, e: Expression): Option[Type] = e match {
        case NExp(n) => Some(NType)
        case BExp(b) => Some(BType)
        case Var(id) => Gamma.get(id)
        case Lam(v, t, b) =>
            getType(K, Gamma + (v.id -> t), b).map {
                bt => FunType(t, bt)
            }
        case FamFun(path, name) =>
            computeLinkage(K, path.get).funs.get(name) match {
                case Some(sig) => Some(sig.t)
                case _ => None
            }
        case FamCases(path, name) =>
            computeLinkage(K, path.get).cases.get(name) match {
                case Some(sig) => Some(sig.t)
                case _ => None
            }
        case App(e1, e2) => {
            val t1 = getType(K, Gamma, e1) match {
                case Some(t: FunType) => t
                case _ => return None
            }
            val t2 = getType(K, Gamma, e2) match {
                case Some(t) => t
                case _ => return None
            }
            if t1.input == t2 then Some(t1.output) else None
        }
        case Rec(fields) => {
            val types = fields.mapValues {
                field => getType(K, Gamma, field)
            }.toMap
            if types.exists((_, t) => t.isEmpty) then None
            Some(RecType(types.mapValues(t => t.get).toMap))
        }
        case Proj(e, name) => getType(K, Gamma, e).flatMap {
            case RecType(fields) => fields.get(name)
            case _ => None
        }
        case Inst(t, rec) => {
            val fields = computeLinkage(K, t.path.get).types.get(t.name) match {
                case Some(typeDefn) => typeDefn.typeBody.defn.get.fields
                case _ => return None
            }
            if fields.keySet != rec.fields.keySet then return None
            if fields.forall(
                (name, t) => getType(K, Gamma, rec.fields.get(name).get) == Some(t)
            ) then Some(t) else None
        }
        // TODO: Do some extra checks.
        case InstADT(t, cname, rec) =>
            val cases = computeLinkage(K, t.path.get).adts.get(t.name) match {
                case Some(adtDefn) => adtDefn.adtBody.defn.get
                case _ => return None
            }
            val fields = cases.get(cname) match {
                case Some(rec) => rec.fields
                case _ => return None
            }
            if fields.keySet != rec.fields.keySet then return None
            if fields.forall(
                (name, t) => getType(K, Gamma, rec.fields.get(name).get) == Some(t)
            ) then Some(t) else None
        case Match(e, c, r) =>
            val t = getType(K, Gamma, e) match {
                case Some(t: PathType) => t
                case _ => return None
            }
            val funType = computeLinkage(K, c.path.get).cases.get(c.name) match {
                case Some(sig) => 
                    if sig.mt != t then return None
                    sig.t
                case _ => return None
            }
            if getType(K, Gamma, r) != Some(funType.input) then return None
            Some(funType.output)
        case IfThenElse(condExpr, ifExpr, elseExpr) =>
            if getType(K, Gamma, condExpr) != BType then return None
            val ifType = getType(K, Gamma, ifExpr) match {
                case Some(t) => Some(t)
                case _ => return None
            }
            val elseType = getType(K, Gamma, elseExpr) match {
                case Some(t) => Some(t)
                case _ => return None
            }
            if ifType == elseType then ifType else None
    }
}
