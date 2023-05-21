import PersimmonSyntax.*
import PersimmonTyping.*
import PersimmonLinkages.*
import PrettyPrint.*


object PersimmonWF {

  // Well-formedness of definitions
  // the top level rule recursively checks 
  // the definition linkage for path prog
  def wfDef(K: PathCtx, lkg: DefinitionLinkage): Boolean = debug(s"wfDef($K,$lkg)", {
    // Notes:
    // - K here corresponds to sp::K in WF-FamDef the paper.
    // - lkg.self corresponds to self(sp.A) in WF-FamDef in the paper.
    // TODO: Is this correct? The way rule EC-Nest is laid out makes me unsure.

    // WF runs on an incomplete, just parsed prog linkage.
    // all lkg.self paths are self-paths
    assert(lkg.self.isInstanceOf[Sp])
    var selfpath = lkg.self.asInstanceOf[Sp].sp
    
    if ancestors(selfpath).contains(selfpath) then false
    else {
      val K_prime = selfpath :: K
      val L_S = computeTypLinkage(Sp(selfpath))

      val exhaustive = exhaustivityCheck(K_prime, L_S)  

      val wfNested = lkg.nested.forall { (_, nested_lkg) => wfDef(K_prime, nested_lkg) }

      val wfTypes = lkg.types.forall { (name, td) =>
        !lkg.adts.contains(name) && 
        ( if td.marker == Eq then wfTypDef(K_prime, td)
          else lkg.defaults.contains(name) && wfTypDefExt(K_prime, td, lkg.defaults(name)))
      }

      val wfAdts = lkg.adts.forall { (name, adt) => !lkg.types.contains(name) && wfAdtDef(K_prime, adt) }

      val wfFuns = lkg.funs.forall { (_, fd) => wfFunDef(K_prime, fd) } 

      val wfCases = lkg.cases.forall { (_, cd) => wfCasesDef(K_prime, cd) }

      // definition is WF if all hold
      exhaustive && wfNested && wfTypes && wfAdts && wfFuns && wfCases
    }
  })

  // this recursively gets all paths from the program 
  // by traversing the typing linkage for prog
  def allPathsContext(): List[SelfPath] = {
    var lkg = computeTypLinkage(Sp(Prog))
    collectAllPathsWithin(lkg)
  }

  def collectAllPathsWithin(lkg: TypingLinkage): List[SelfPath] = {
    assert(lkg.self.isInstanceOf[Sp])
    var selfpath = lkg.self.asInstanceOf[Sp].sp

    var lstResult = selfpath :: List()
    for ((famName, nestLkg) <- lkg.nested) {
      lstResult = collectAllPathsWithin(nestLkg) ++ lstResult
    }
    lstResult
  }

  // ancestors function
  def ancestors(p: SelfPath): List[SelfPath] = {
    var currLkg = computeTypLinkage(Sp(p))
    currLkg.getSuperPath() match {
      case Some(p) => relativizePath(p) :: ancestors(relativizePath(p))
      case None => return List()
    } 
  }

  // well-formedness of type definitions
  def wfTypDef(K: PathCtx, td: TypeDefn): Boolean = debug(s"wfTypDef($K,$td)", {
    wfType(K, td.typeBody);
  })

  // well-formedness of type extensions and their defaults
  def wfTypDefExt(K: PathCtx, td: TypeDefn, dd: DefaultDefn): Boolean = debug(s"wfType($K,$td,$dd)", {
    wfType(K, td.typeBody) && dd.defaultBody.fields.forall { (name, e) =>
      td.typeBody.fields.contains(name) &&
      hasType(K, Map(), e, td.typeBody.fields(name))
    }
  })

  // well-formedness of ADT definitions
  def wfAdtDef(K: PathCtx, adt: AdtDefn): Boolean = debug(s"wfAdtDef($K,$adt)", {
    adt.adtBody.forall { (name, rec) => wfType(K, rec) }
  })

  // well-formedness of functions
  def wfFunDef(K: PathCtx, fd: FunDefn): Boolean = debug(s"wfFunDef($K,$fd)", {
    val typeWF = wfType(K, fd.t)
    val bodyWF = hasType(K, Map(), fd.funBody, fd.t)

    typeWF && bodyWF
  })

  // well-formedness of cases definitions
  def wfCasesDef(K: PathCtx, cd: CasesDefn): Boolean = debug(s"wfCasesDef($K,$cd)", {
    val L_S = computeTypLinkage(cd.matchType.path.get);
    val matchTypeExists = L_S.adts.contains(cd.matchType.name)
    val typeWF = wfType(K, cd.t)
    val bodyWT = debug(s"hasType($K, Map(), ${printExp(cd.casesBody)}, ${printType(cd.t)}) vs ${getType(K, Map(), cd.casesBody).map(printType)}", hasType(K, Map(), cd.casesBody, cd.t))

    val adtDefinition = L_S.adts(cd.matchType.name).adtBody
    val allHandlerTypesValid = (cd.t.output match {
      case RecordType(rec) => 
        (rec.forall { (constructorName, handlerType) =>
          // all handler types must be arrow types
          if(!handlerType.isInstanceOf[FunType]) then debug("expect arrow type", false)
          else {
            // all handler types must have the same output type
            if (handlerType.asInstanceOf[FunType].output != 
                rec.head._2.asInstanceOf[FunType].output) 
            then debug("expect same output type", false)
            else {
              val constructorArgsInLkg = adtDefinition.get(constructorName)
              constructorArgsInLkg match {
                // if no such constructor in type definition
                case None => debug("missing constructor", false)
                case Some(t) => 
                  // the handler input type must match the 
                  // constructor arguments in ADT definition
                  debug("handler mismatch", handlerType.asInstanceOf[FunType].input == t)
              }
            }
          }
        })
      case _ => debug("output type must be a record type", false) // output type for cases sig is not a record type
    })

    // WF if all hold
    debug("matchTypeExists", matchTypeExists) &&
    debug("allHandlerTypesValid", allHandlerTypesValid) &&
    debug("typeWF", typeWF) &&
    debug("bodyWT", bodyWT)
  })

  //rule EC-Nest
  def exhaustivityCheck(K: PathCtx, lkg: TypingLinkage): Boolean = debug(s"exhaustivityCheck($K,$lkg)", {
    val currentFamChecked = (lkg.cases.forall { (name, cases) => {
      val L_S_prime = computeTypLinkage(cases.matchType.path.get)
      val adtName = cases.matchType.name
      val adtExists = L_S_prime.adts.contains(adtName)

      val handlers = cases.t.output.asInstanceOf[RecordType].fields
      val adtDefinition = L_S_prime.adts(adtName).adtBody
      // for each constructor in the ADT definition,
      // it has a corresponding handler in the output type
      // with the proper input type
      val allConstructorsHandled = adtDefinition.forall {
        (constructorName, arguments) => 
          val constructorHandled = handlers.contains(constructorName)
          if (!constructorHandled) then false else {
            val argsMatch = (handlers.get(constructorName).get.asInstanceOf[FunType].input == arguments)

            argsMatch
          }
      }

      adtExists && allConstructorsHandled
    }})
    

    val nestedFamsChecked = lkg.nested.forall { (name, A) => {
      assert(A.self.isInstanceOf[Sp])
      var selfpath = A.self.asInstanceOf[Sp].sp
      val K_prime = List(selfpath) ++ K
      val L_S_prime_prime = computeTypLinkage(Sp(selfpath))
      exhaustivityCheck(K_prime, L_S_prime_prime)
    }}

    currentFamChecked && nestedFamsChecked
  })


  // Well-formedness of types
  def wfType(K: PathCtx, t: Type): Boolean = debug(s"wfType($K,$t)", t match {
    case NType => true
    case BType => true
    case FunType(input, output) => wfType(K, input) && wfType(K, output)
    case PathType(path, name) =>
      val linkage = computeTypLinkage(path.get)
      linkage.types.contains(name) || linkage.adts.contains(name)
    case RecordType(fields) =>
      fields.forall { (name, t) => wfType(K, t) }
  })

  def debug(msg: => String, b: Boolean): Boolean = {
    if (!b) {
      println("false at "+msg)
    }
    b
  }
}
