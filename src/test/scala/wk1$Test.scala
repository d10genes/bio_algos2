import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Gen._
import wk1.wk1._

/**
 * Created by williambeard on 2/18/15.
 */

object wk1$Test extends Properties("String") {
  val e = Empty: Trie[Char]
  property("insertList") = forAll { (a: String) =>
    val lst = a.toList
    e.insertList(lst) == e.insertList(lst).insertList(lst)
  }
  def genDNA: Gen[DNA] = oneOf(A, C, G, T)
  def genSlice(ds: List[DNA], minLen: Int=5, maxLen: Int=10): Gen[List[DNA]] = {
    for {
      start <- Gen.choose(0, ds.length - maxLen)
      len <- Gen.choose(minLen, maxLen)
    }
    yield ds.slice(start, start + len)
  }

  def genPatSet(n: Int=100, substrs: Int=5): Gen[(List[DNA], List[List[DNA]])] = {
    for {
      text <- Gen.listOfN(n, genDNA)
      subs <- Gen.listOfN(substrs, genSlice(text))
    } yield (text, removeSubs(subs))
  }

  property("searchList") = forAll(genPatSet()) { (tup: (List[DNA], List[List[DNA]])) =>
    //    val lst = a.toList
    println("==>\n")
    val (text, subs) = tup
    val nfound = text.tails.map((x: List[DNA]) => dumbCheck(x, subs)).count((x: Boolean) => x)
    println(s"nfound: $nfound len: ${subs.length}")
    if (nfound < subs.length) println(s"text: $text\nsubs: $subs")
    else print()
    nfound >= subs.length
//        text.tails.forall(!dumbCheck(_, subs))
      }

//  property("insertList") = forAll { (a: String, as: List[String]) =>
////    val lst = a.toList
//    val nukes = "ATCG".toList
//    val ass = a::as
//    val tr = insertStrs(as)
////    val tr = insertStrs(List("ATAGA", "ATC", "GAT"))
////    e.insertList(lst) == e.insertList(lst).insertList(lst)
//  }

}