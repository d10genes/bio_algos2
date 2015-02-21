import org.scalatest.FunSuite
import wk1.wk1._

/**
 * Created by williambeard on 2/21/15.
 */
class wk1Sc$Test extends FunSuite {
  test("""removeSubs should remove dupes, Nils and prefixes of other members
         | or there are more than n different chars""") {
    val dupes = List(List(A, A, G), List(A, A, G, C), List(A, G, C), List(A, A, G, C), Nil)
    val noDupes = List(List(A, A, G, C), List(A, G, C))
    assert(removeSubs(dupes) == noDupes)

  }
}
