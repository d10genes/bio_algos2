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

  case class Trie[A](content: Option[A], children: List[Trie[A]]) {
    //    def apply(a: A): Trie[A] = new Trie(Some(a), Nil)
    def showContent: String = content match {
      case None => "None"
      case Some(a) => a.toString
    }

    override def toString = s"N($showContent) -> ${showList(children)}"

    def addNode(child: Trie[A]): Trie[A] = content match {
      case None => child //throw Exception //new Node[A](, ) //throw Exception
      case Some(a) => new Trie[A](content, child :: children)
    }
  }


  def Tr[A](a: A): Trie[A] = new Trie(Some(a), Nil)
  def Empty[A]: Trie[A] = new Trie(None, Nil)
  def showList[A](xs: List[A]): String = {
    xs match {
      case Nil => ""
      case ys => s"[${ys.map(_.toString).mkString(", ")}]"
    }
  }

  def insertList[A](t: Trie[A], xs: List[A]): Trie[A] = xs match {
    case Nil => t
    case y::ys => t.children.span(_.content != Some(y)) match {
      case (neqs, Nil)     => new Trie(t.content, neqs:::(insertList(Tr(y), ys)::Nil))
      case (neqs, eq::rst) => new Trie(t.content, neqs:::(insertList(eq,    ys)::rst))
    }
  }
}
