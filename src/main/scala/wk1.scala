package wk1
//package com.beard.bio2_scala.utils

/**
 * Created by williambeard on 2/17/15.
 */
object wk1 {
//  sealed trait Trie[A] {
//    val content: Option[A]
////    def value: Option[A] = this match {
////      case n: Node[A] => Some(n.content)
////      case Empty      => None
////    }
//    def addNode(ch: Trie[A]): Trie[A] = {
//      this match {
//        case Empty => ch //throw Exception //new Node[A](, ) //throw Exception
//        case Node(c, chs) => Node(c, ch::chs)
//  }
//}
//  }

  case class Trie[A](content: Option[A], children: List[Trie[A]]) extends Traversable[A]{
    //    def apply(a: A): Trie[A] = new Trie(Some(a), Nil)

    def foreach[U](f: A => U) = {
      content.foreach(f)
      children.foreach(_.foreach(f))
    }


    def showContent: String = content match {
      case None => "None"
      case Some(a) => a.toString
    }

    override def toString = s"N($showContent) -> ${showList(children)}"

    def addNode(child: Trie[A]): Trie[A] = content match {
      case None => child //throw Exception //new Node[A](, ) //throw Exception
      case Some(a) => new Trie[A](content, child :: children)
    }
    def insertList(xs: List[A]): Trie[A] = xs match {
      case Nil => this
      case y::ys => children.span(_.content != Some(y)) match {
        case (neqs, Nil)     => new Trie(content, neqs:::(Tr(y).insertList(ys)::Nil))
        case (neqs, eq::rst) => new Trie(content, neqs:::(eq.insertList(    ys)::rst))
      }
    }
  }

//  type Trip = (Int, Int, Char)
  type Trip[A] = (Int, Int, A)
  type Rec[A] = (Int, Int, List[Trip[A]])


  def adjLst[A](tri: Trie[A]): List[Trip[A]] = {
    def recurse(rec: Rec[A], tr: Trie[A]): Rec[A] = {
//      println(s"rec: $rec, tr: $tr")
      val (mx, par, ts) = rec
      val curN = mx + 1
//      val curTrip = (par, curN, tr.content)
      val nxtRec = (curN, curN, Nil: List[Trip[A]])
      val (chldMx, _, chldTs) = tr.children.foldLeft(nxtRec)(recurse)
      val retTrips = tr.content match {
        case None => chldTs
        case Some(x) => (par, curN, x)::chldTs
      }
      (chldMx, par, retTrips:::ts)
    }
  recurse((-1, -1, Nil), tri)._3
  }

  def insertStrs(ss: List[String]): Trie[Char] = {
    ss.map(_.toList).foldLeft(Empty: Trie[Char])(_.insertList(_))
  }


  def fmtAdjLst(xs: List[Trip[Char]]) = {
    val f = (t: Trip[Char]) => s"${t._1}->${t._2}:${t._3}"
    xs.map(f).mkString("\n")
  }

  def Tr[A](a: A): Trie[A] = new Trie(Some(a), Nil)
  def Empty[A]: Trie[A] = new Trie(None, Nil)
  def showList[A](xs: List[A]): String = {
    xs match {
      case Nil => ""
      case ys => s"[${ys.map(_.toString).mkString(", ")}]"
    }
  }

//  def insertList[A](t: Trie[A], xs: List[A]): Trie[A] = xs match {
//    case Nil => t
//    case y::ys => t.children.span(_.content != Some(y)) match {
//      case (neqs, Nil)     => new Trie(t.content, neqs:::(insertList(Tr(y), ys)::Nil))
//      case (neqs, eq::rst) => new Trie(t.content, neqs:::(insertList(eq,    ys)::rst))
//    }
//  }
}
