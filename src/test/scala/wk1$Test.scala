import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Gen._
import wk1.wk1._

/**
 * Created by williambeard on 2/18/15.
 */

object wk1$Test extends Properties("Trie") {
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

  def countFound[A](text: List[A], pats: List[List[A]], fn: (List[A], List[List[A]]) => Boolean): Int = {
    text.tails.map((x: List[A]) => fn(x, pats)).count(bid)
  }
  property("dumbCheck") = forAll(genPatSet()) { (tup: (List[DNA], List[List[DNA]])) =>
//    println("==>\n")
    val (text, subs) = tup
    val nfound = text.tails.map((x: List[DNA]) => dumbCheck(x, subs)).count(bid)
//    println(s"nfound: $nfound len: ${subs.length}")
//    if (nfound < subs.length) println(s"text: $text\nsubs: $subs")
//    else print()
    nfound >= subs.length // can be greater if dupe sequences in text
  }

  property("search vs dumbCheck") = forAll(genPatSet()) { (tup: (List[DNA], List[List[DNA]])) =>
    //    println("==>\n")
    val (text, subs) = tup
//    val nfound = text.tails.map((x: List[DNA]) => dumbCheck(x, subs)).count(bid)
    val nfound = countFound(text, subs, dumbCheck)
    val nfound2 = countFound(text, subs, trieCheck)

    //    println(s"nfound: $nfound len: ${subs.length}")
    //    if (nfound < subs.length) println(s"text: $text\nsubs: $subs")
    //    else print()
//    nfound >= subs.length // can be greater if dupe sequences in text
    if (nfound != nfound2) {println(s"nfound: $nfound\nnfound2: $nfound2")
      println(s"text: $text\nsubs: $subs")
    } else ()

//    println(s"nfound: $nfound\nnfound2: $nfound2")
    nfound == nfound2
  }
}