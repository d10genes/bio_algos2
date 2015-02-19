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

def insertList[A](t: Trie[A], xs: List[A]): Trie[A] = xs match {
  case Nil => t
  case y::ys => t.children.span(_.content != Some(y)) match {
    case (neqs, Nil) => new Trie(t.content, neqs:::(insertList(Tr(y), ys)::Nil))
    case (neqs, eq::rst) => new Trie(t.content, neqs:::(insertList(eq, ys)::rst))
  }
}

val ns = ll.map(Tr)
ss.head

ns.span(_.content != Some('T'))
val n1 = insertList(Empty: Trie[Char], ll)
val n2 = insertList(n1, ll)
