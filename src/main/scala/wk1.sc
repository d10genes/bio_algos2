//import wk1._
import wk1.wk1._
//import com.beard.bio2_scala.utils.wk1o._

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

e.addNode(t)
t.addNode(t.addNode(t))

def addString(t: Trie[Char], s: String): Trie[Char] = s match {
  case "" => t
//  case _ =>
}

//def insertList[A](t: Trie[A], xs: List[A]): Trie[A] = xs match {
//  case Nil => t
//  case y::ys => t.children.span(_.content != Some(y)) match {
//    case (neqs, Nil) => new Trie(t.content, neqs:::(insertList(Tr(y), ys)::Nil))
//    case (neqs, eq::rst) => new Trie(t.content, neqs:::(insertList(eq, ys)::rst))
//  }
//}

val ns = ll.map(Tr)
ss.head

ns.span(_.content != Some('T'))
val n1 = e.insertList(ll)
val n2 = n1.insertList(ll)
val n3 = n1.insertList("ATC".toList)
//def trav[A](t: Trie[A]): List[A] =
//n3.foreach(print)
//((1 until 30), n3: Trie[Char]).zipped.map((x, y)=> print(y))
n3.map(print)



val tr = insertStrs(List("ATAGA", "ATC", "GAT"))
fmtAdjLst(adjLst(tr))

//adjLst(n3.insertList("GAT".toList)).map()
//def fmtLst()
//0->1:A
//1->2:T
//2->3:A
//3->4:G
//4->5:A
//2->6:C
//0->7:G
//7->8:A
//8->9:T