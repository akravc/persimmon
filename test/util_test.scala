// import org.scalatest.funsuite.AnyFunSuite
// import PersimmonSyntax._
// import PersimmonLinkages._ 
// import PersimmonTyping._ 
// import TestParser._
// import PrettyPrint._
// import PersimmonWF._
// import PersimmonUtil._
// import scala.language.postfixOps
// import java.io.PrintWriter
// import java.io.File
// import scala.io.Source

// class UtilTesting extends AnyFunSuite {

//   def readFile(filename: String): String = { 
//     return Source.fromFile(filename).getLines.mkString
//   }

//   test("util - cases input type") {
//     val fam = readFile("res/peano")
//     PersimmonLinkages.p = fam
//     val linkage = TestParser.parseProgramDefLink(fam)
//     printLkg(linkage, "")
//     print(getCasesInpType(linkage.nested("Peano").cases("eqNat_cases").t))
//   }

//   test("util - cases output type") {


//   }
// }