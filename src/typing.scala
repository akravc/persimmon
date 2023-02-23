import PersimmonSyntax.*

object PersimmonTyping {
    def getType(Expression e): Type = e match {
        case NExp(n) => NType
        case BExp(b) => BType
        // TODO: Get type of variable from context
        case Var(id) => throw new Exception("Typing of variables not yet implemented.")
        // TODO: Add 'v' to context and get type of 'b' within modified context
        case Lam(v, t, b) => FunType(t, getType(b))
    }
}
