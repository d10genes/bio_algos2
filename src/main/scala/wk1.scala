package wk1
//package com.beard.bio2_scala.utils

/**
 * Created by williambeard on 2/17/15.
 */
object wk1 {

  case class Trie[A](content: Option[A], children: List[Trie[A]]) extends Traversable[A] {

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
      case y :: ys => children.span(_.content != Some(y)) match {
        case (neqs, Nil) => new Trie(content, neqs ::: (Tr(y).insertList(ys) :: Nil))
        case (neqs, eq :: rst) => new Trie(content, neqs ::: (eq.insertList(ys) :: rst))
      }
    }
    def search(txt: List[A]): Boolean = (txt, this.children, this.content) match {
      case (Nil, Nil, _) => true
      case (Nil, _, _) => false
      case (_, Nil, _) => true
      case (_, _, Some(x)) => if (x == txt.head)
        this.children.exists(_.search(txt.tail))
      else false
      case (_, _, None) => this.children.exists(_.search(txt))
    }
  }

  type Trip[A] = (Int, Int, A)
  type Rec[A] = (Int, Int, List[Trip[A]])

  def adjLst[A](tri: Trie[A]): List[Trip[A]] = {
    def recurse(rec: Rec[A], tr: Trie[A]): Rec[A] = {
      val (mx, par, ts) = rec
      val curN = mx + 1
      val nxtRec = (curN, curN, Nil: List[Trip[A]])
      val (chldMx, _, chldTs) = tr.children.foldLeft(nxtRec)(recurse)
      val retTrips = tr.content match {
        case None => chldTs
        case Some(x) => (par, curN, x) :: chldTs
      }
      (chldMx, par, retTrips ::: ts)
    }
    recurse((-1, -1, Nil), tri)._3
  }

  val bid = (x: Boolean) => x
  def removeSubs[A](xss: List[List[A]]): List[List[A]] = {
    val dedup = xss.distinct.filter(_.length > 0)
    dedup.filter((y) => dedup.map((x) => x.startsWith(y)).count(bid) == 1)
//    dedup.filter((y) => dedup.map((x) => x.containsSlice(y)).count(bid) == 1)
  }

  // Ans Util Funcs
  def insertStrs(ss: List[String]): Trie[Char] = {
    ss.map(_.toList).foldLeft(Empty: Trie[Char])(_.insertList(_))
  }

  def fmtAdjLst(xs: List[Trip[Char]]) = {
    val f = (t: Trip[Char]) => s"${t._1}->${t._2}:${t._3}"
    xs.map(f).mkString("\n")
  }

  def runTrie(fn: String) = {
    import java.io._
    val lines = scala.io.Source.fromFile(fn).getLines().toList  //.mkString
//    println(lines)
//  def runTrie(lines: List[String]) = {
    val alst = fmtAdjLst(adjLst(insertStrs(lines)))

    val fout = s"$fn.ans.txt"
    val file = new File(fout)
//    val file = new File("src/data/TrieConstruction.ans.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(alst)
    bw.close()
    println(fout)
  }

  // Type Aux. funcs
  def Tr[A](a: A): Trie[A] = new Trie(Some(a), Nil)

  def Empty[A]: Trie[A] = new Trie(None, Nil)

  def showList[A](xs: List[A]): String = {
    xs match {
      case Nil => ""
      case ys => s"[${ys.map(_.toString).mkString(", ")}]"
    }
  }

  sealed trait DNA // extends Nucleotide
  case object A extends DNA
  case object C extends DNA
  case object G extends DNA
  case object T extends DNA

  def dumbCheck[A](txt: List[A], pats: List[List[A]]): Boolean = {
    def comp(txt: List[A], pat: List[A]): Boolean = {
      if (pat.length > txt.length) false
      else (pat, txt).zipped.forall(_ == _)
    }
//    println(s"pats: ${showList(pats.map(showList))}\ntxt: ${showList(txt)}")
    (pats, txt) match {

      case (Nil, _) => false
      case (_, Nil) => false
      case _ => pats.exists(comp(txt, _))
    }
  }
}

object ans1 {
  import wk1._

//  val lines = scala.io.Source.fromFile("src/data/w1tst.txt").getLines().toList  //.mkString

  def main (args: Array[String]) {
//    runTrie("src/data/w1tst.txt")
    runTrie("src/data/dataset_294_4.txt")
//    runTrie("src/data/TrieConstruction.txt")

//    println(runTrie("src/data/w1tst.txt"))
//    println(runTrie("src/data/w1tst.txt"))
    println("Done.")
  }

}