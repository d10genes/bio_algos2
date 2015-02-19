//import wk1._
import wk1.wk1._
//import com.beard.bio2_scala.utils.wk1o._

val t = Trie('a')
val e = Empty

val l = List(1,2)
//l.map(_.toString).mkString(", ")
showList(l)

//def addNode[A](ch: Trie[A], pa: Trie[A]): Trie[A] = {
//  pa.ch
//}

//t.children

val lst = "ATAGA"

addNode(e, t)
addNode(t, t)