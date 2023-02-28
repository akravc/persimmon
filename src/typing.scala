import PersimmonSyntax.*

object PersimmonTyping {

    type TypingCtx = Map[String, Type] // Gamma
    type PathCtx = List[Path] // K

    def getType(e: Expression): Type = e match {
        case NExp(n) => NType
        case BExp(b) => BType
        // TODO: Get type of variable from context
        case Var(id) => throw new Exception("Typing of variables not yet implemented.")
        // TODO: Add 'v' to context and get type of 'b' within modified context
        case Lam(v, t, b) => FunType(t, getType(b))
        // TODO
        case FamFun(path, name) => throw new Exception("Typing of family functions not yet implemented.")
        case FamCases(path, name) => throw new Exception("Typing of family functions not yet implemented.")
        case App(e1, e2) => {
            val t1 = getType(e1)
            val t2 = getType(e2)
            t1 match {
                case FunType(input, output) => {
                    if input == t2 then t1
                    else throw new Exception("Function applied to expression of wrong type.")
                }
                case _ => throw new Exception("Attempted to apply a non-function as a function.")
            }
        }
        case Rec(fields) => {
            RecType(fields.mapValues(field => getType(field)).toMap)
        }
        case Proj(e, name) => {
            val t = getType(e)
            t match {
                case RecType(fields) => {
                    fields.get(name) match {
                        case Some(field) => field
                        case None => throw new Exception("Attempted to access a non-existent field.")
                    }
                }
                case _ => throw new Exception("Attempted to apply a non-function as a function.")
            }
        }
        // TODO: Do some extra checks.
        case Inst(t, rec) => t
        // TODO: Do some extra checks.
        case InstADT(t, cname, rec) => t
        case Match(e, c, r) => throw new Exception("Typing of match expressions not yet implemented.")
        case IfThenElse(condExpr, ifExpr, elseExpr) => {
            if getType(condExpr) != BType then throw new Exception("Attempted to perform an if-then-else operation on an expression which was not a boolean.")
            val ifType = getType(ifExpr)
            val elseType = getType(elseExpr)
            if ifType != elseType then throw new Exception("Branches of if-then-else statement have conflicting types")
            ifType
        }
    }
}
