import PersimmonSyntax.*
import PersimmonTyping.*

object PersimmonLinkages {

    def computeLinkage(K: PathCtx, a: Path): TypingLinkage = {
        return TypingLinkage(Sp(Prog), Prog, None, Map(), Map(), Map(), Map(), Map(), Map())
    }

}