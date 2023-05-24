import PersimmonSyntax.*
import PersimmonLinkages.*
import PersimmonWF.*
import PrettyPrint.*

object PersimmonReduction {
  def reduce(K: PathCtx, e: Expression): Option[Expression] = e match {
    case NExp(n) => None
    case BExp(b) => None
    case Var(id) => throw Exception("Expression evaluated to unbound variable")
    case Lam(v, t, body) => None
    case FamFun(path, name) => {
      val linkage = computeDefLinkage(path.get)
      Some(linkage.funs(name).funBody)
    }
    case FamCases(path, name) => {
      val linkage = computeDefLinkage(path.get)
      Some(linkage.cases(name).casesBody)
    }
    // case App(e1, e2) => reduce(K, e1) match {
    //   case Some(e1Prime) => App(e1Prime, e2)
    //   case None => reduce(K, e2) match {
    //     case Some(e2Prime) => App(e1Prime, e2Prime)
    //     case None => e1 match {
    //       case Lam(v, t, body) => 
    //       case _ => throw Exception("Function did not reduce to lambda expression")
    //     }
    //   }
    // }
    case IfThenElse(condExpr, ifExpr, elseExpr) => reduce(K, condExpr) match {
      case Some(condExprPrime) => Some(IfThenElse(condExprPrime, ifExpr, elseExpr))
      case None => condExpr match {
        case BExp(b) => if b then Some(ifExpr) else Some(elseExpr)
        case _ => throw Exception("Conditional expression did not reduce to boolean")
      }
    }
    case _ => throw Exception("Not implemented")
  }
}
