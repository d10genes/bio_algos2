package wk1
//package com.beard.bio2_scala.utils

/**
 * Created by williambeard on 2/17/15.
 */
object wk1 {
  sealed trait Trie[A] {
    val content: Option[A]
//    def value: Option[A] = this match {
//      case n: Node[A] => Some(n.content)
//      case Empty      => None
//    }
    def addNode(ch: Trie[A]): Trie[A] = {
      this match {
        case Empty => ch //throw Exception //new Node[A](, ) //throw Exception
        case Node(c, chs) => Node(c, ch::chs)
  }
}
  }
  case object Empty extends Trie[Nothing] {
    override def toString = "."
    val content = None
  }
  case class Node[A](cont: A, children: List[Trie[A]]) extends Trie[A] {
    override def toString = s"N($cont) -> ${showList(children)}"
    val content = Some(cont)
  }

  object Trie {
    def apply[A](a: A): Trie[A] = new Node(a, Nil)
  }

  def showList[A](xs: List[A]): String = {
    xs match {
      case Nil => ""
      case ys => s"[${ys.map(_.toString).mkString(", ")}]"
    }
  }


}
