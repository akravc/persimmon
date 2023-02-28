import PersimmonSyntax.*

object PersimmonTyping {
    type TypingCtx = Map[String, Type] // Gamma
    type PathCtx = List[Path] // K
    
    def getType(e: Expression): Option[Type] = e match {
        case NExp(n) => Some(NType)
        case BExp(b) => Some(BType)
        // TODO: Get type of variable from context
        case Var(id) => throw new Exception("Typing of variables not yet implemented.")
        // TODO: Add 'v' to context and get type of 'b' within modified context
        case Lam(v, t, b) => {
            val bt = getType(b) match {
                case Some(t) => t
                case _ => return None
            }
            Some(FunType(t, bt))
        }
        // TODO
        case FamFun(path, name) => throw new Exception("Typing of family functions not yet implemented.")
        case FamCases(path, name) => throw new Exception("Typing of family functions not yet implemented.")
        case App(e1, e2) => {
            val (input, output) = getType(e1) match {
                case Some(FunType(input, output)) => (input, output)
                case _ => return None
            }
            val t2 = getType(e2) match {
                case Some(t) => t
                case _ => return None
            }
            if input == t2 then Some(output)
            else None
        }
        case Rec(fields) => {
            val option_types = fields.mapValues(field => getType(field)).toMap
            if option_types.forall((_, t) => t.isEmpty) then None
            val types = option_types.mapValues(t => t.get).toMap
            Some(RecType(types))
        }
        case Proj(e, name) => {
            val fields = getType(e) match {
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
            if getType(condExpr) != BType then return None
            val ifType = getType(ifExpr) match {
                case Some(t) => Some(t)
                case _ => return None
            }
            val elseType = getType(elseExpr) match {
                case Some(t) => Some(t)
                case _ => return None
            }
            if ifType != elseType then return None
            ifType
        }
    }
}
