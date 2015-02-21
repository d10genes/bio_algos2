import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
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

}

