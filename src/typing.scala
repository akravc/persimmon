import PersimmonSyntax.*

object PersimmonTyping {
    type TypingCtx = Map[String, Type] // Gamma
    type PathCtx = List[Path] // K
    
    def getType(k: PathCtx, gamma: TypingCtx, e: Expression): Option[Type] = e match {
        case NExp(n) => Some(NType)
        case BExp(b) => Some(BType)
        case Var(id) => gamma.get(id) match {
            case Some(t) => Some(t)
            case _ => None
        }
        case Lam(v, t, b) => {
            val bt = getType(k, gamma + (v.id -> t), b) match {
                case Some(t) => t
                case _ => return None
            }
            Some(FunType(t, bt))
        }
        // TODO
        case FamFun(path, name) => throw new Exception("Typing of family functions not yet implemented.")
        case FamCases(path, name) => throw new Exception("Typing of family functions not yet implemented.")
        case App(e1, e2) => {
            val (input, output) = getType(k, gamma, e1) match {
                case Some(FunType(input, output)) => (input, output)
                case _ => return None
            }
            val t2 = getType(k, gamma, e2) match {
                case Some(t) => t
                case _ => return None
            }
            if input == t2 then Some(output)
            else None
        }
        case Rec(fields) => {
            val option_types = fields.mapValues(field => getType(k, gamma, field)).toMap
            if option_types.forall((_, t) => t.isEmpty) then None
            val types = option_types.mapValues(t => t.get).toMap
            Some(RecType(types))
        }
        case Proj(e, name) => {
            val fields = getType(k, gamma, e) match {
                case Some(RecType(fields)) => fields
                case _ => return None
            }
            fields.get(name) match {
                case Some(field) => Some(field)
                case _ => return None
            }
        }
        // TODO: Do some extra checks.
        case Inst(t, rec) => Some(t)
        // TODO: Do some extra checks.
        case InstADT(t, cname, rec) => Some(t)
        case Match(e, c, r) => throw new Exception("Typing of match expressions not yet implemented.")
        case IfThenElse(condExpr, ifExpr, elseExpr) => {
            if getType(k, gamma, condExpr) != BType then return None
            val ifType = getType(k, gamma, ifExpr) match {
                case Some(t) => Some(t)
                case _ => return None
            }
            val elseType = getType(k, gamma, elseExpr) match {
                case Some(t) => Some(t)
                case _ => return None
            }
            if ifType != elseType then return None
            ifType
        }
    }
}
