import PersimmonSyntax.*
import PersimmonLinkages.*
import PersimmonWF.*
import PersimmonUtil.*
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
    case App(e1, e2) => reduce(K, e1) match {
      case Some(e1Prime) => Some(App(e1Prime, e2))
      case None => reduce(K, e2) match {
        case Some(e2Prime) => Some(App(e1, e2Prime))
        case None => e1 match {
          case Lam(v, t, body) => Some(subVarInExp(body, e2, v))
          case _ => throw Exception("Function did not reduce to lambda expression")
        }
      }
    }
    case Plus(e1, e2) => reduce(K, e1) match {
      case Some(e1Prime) => Some(Plus(e1Prime, e2))
      case None => reduce(K, e2) match {
        case Some(e2Prime) => Some(Plus(e1, e2Prime))
        case None => e1 match {
          case NExp(n1) => e2 match {
            case NExp(n2) => Some(NExp(n1+n2))
            case _ => throw Exception("Right side of plus did not reduce to natural number")
          }
          case _ => throw Exception("Left side of plus did not reduce to natural number")
        }
      }
    }
    case Record(fields) => {
      val (reduced, newFields) = fields.foldLeft((false, Map[String, Expression]())) {
        case ((reduced, fields), (key, value)) =>
          if reduced then (true, fields + (key -> value))
          else reduce(K, value) match {
            case Some(newValue) => (true, fields + (key -> newValue))
            case None => (false, fields + (key -> value))
          }
      }
      if reduced then Some(Record(newFields)) else None
    }
    case Proj(e, name) => reduce(K, e) match {
      case Some(ePrime) => Some(Proj(ePrime, name))
      case None => e match {
        case Record(fields) => Some(fields(name))
        case Inst(t, rec) => Some(rec.fields(name))
        case _ => throw Exception("Record did not fully reduce")
      }
    }
    case Inst(t, rec) => reduce(K, rec) match {
      case Some(newRec) => Some(Inst(t, newRec.asInstanceOf[Record]))
      case None => None
    }
    case InstADT(t, cname, rec) => reduce(K, rec) match {
      case Some(newRec) => Some(InstADT(t, cname, newRec.asInstanceOf[Record]))
      case None => None
    }
    case Match(e, c, r) => reduce(K, e) match {
      case Some(ePrime) => Some(Match(ePrime, c, r))
      case None => reduce(K, r) match {
        case Some(rPrime) => Some(Match(e, c, rPrime.asInstanceOf[Record]))
        case None => e match {
          case InstADT(t, cname, rec) => Some(App(Proj(App(c, r), cname), rec))
          case _ => throw Exception("Match expression did not reduce to ADT instance")
        }
      }
    }
    case IfThenElse(condExpr, ifExpr, elseExpr) => reduce(K, condExpr) match {
      case Some(condExprPrime) => Some(IfThenElse(condExprPrime, ifExpr, elseExpr))
      case None => condExpr match {
        case BExp(b) => if b then Some(ifExpr) else Some(elseExpr)
        case _ => throw Exception("Conditional expression did not reduce to boolean")
      }
    }
  }
  
  def normalize(K: PathCtx, e: Expression): Expression = reduce(K, e) match {
    case Some(ePrime) => normalize(K, ePrime)
    case None => e
  }
}
