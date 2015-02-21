//import wk1._
import wk1.wk1._
//import com.beard.bio2_scala.utils.wk1o._
//import org.scalacheck._

val t = Tr('a')
val e: Trie[Char] = Empty

val l = List(1,2,3,4)
//l.map(_.toString).mkString(", ")
showList(l)

//def addNode[A](ch: Trie[A], pa: Trie[A]): Trie[A] = {
//  pa.ch
//}
//t.children

val lst = "ATAGA"
val ll = lst.toList
val ss = lst.toSet

val n1 = e.insertList(ll)
val n2 = n1.insertList(ll)
val n3 = n1.insertList("ATC".toList)
//def trav[A](t: Trie[A]): List[A] =
//n3.foreach(print)
//((1 until 30), n3: Trie[Char]).zipped.map((x, y)=> print(y))
n3.map(print)


//val lines = scala.io.Source.fromFile("src/data/w1tst.txt").mkString

val tr = insertStrs(List("ATAGA", "ATC", "GAT"))
//fmtAdjLst(adjLst(tr))
def search[A](t: Trie[A], txt: List[A]): Boolean = (txt, t.children, t.content) match {
  case (Nil, Nil, _) => true
  case (Nil, _, _) => false
  case (_, Nil, _) => true
  case (_, _, Some(x)) => if (x == txt.head)
    t.children.exists(search(_, txt.tail))
    else false
  case (_, _, None) => t.children.exists(search(_, txt))
}

val subs = removeSubs(List(List(A, A, G), List(A, A, G, C), List(A, G, C), List(A, A, G, C)))
val sss = List(A, A, G).containsSlice(List())
val tx = List(A, A, G, G, G, G, C, G, C, A, T, T, A, C, C)
val pts = List(List(C, A, G, T, T, A), List(A, A, G, G, G, G, C, G, C, A))
dumbCheck(tx, pts)
//Gen.
//val xx = (lst, "ATrGAG").zipped.forall(_ == _)
//val sch = search(n3, List('A', 'T', 'C'))
//val sch2 = search(n3, "ATAGA".toList)

