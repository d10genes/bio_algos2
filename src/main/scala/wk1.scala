package wk1
//package com.beard.bio2_scala.utils

/**
 * Created by williambeard on 2/17/15.
 */
object wk1 {

  case class Trie[A](content: Option[A], children: List[Trie[A]]) extends Traversable[A] {
//    def apply(xs: List[A]): Trie[A] = xs.foldLeft(new Trie(None, Nil: List[Trie[A]]))(_.insertList(_))

    def foreach[U](f: A => U) = {
      content.foreach(f)
      children.foreach(_.foreach(f))
    }

    def showContent: String = content match {
      case None => "None"
      case Some(a) => a.toString
    }

    override def toString = s"($showContent) -> ${showList(children)}"

    def showTrie(lvl: Int = 0): String = { //
      val n = this.content match {
        case None => "o"
        case Some(c) => s"-> ($c)"
      }
      val spc = " " * 2 * lvl //+ "->"
      val ns = s"$spc$n"
      this.children match {
        case Nil => ns
        case ts => (ns::ts.map(_.showTrie(lvl + 1))).mkString("\n")
      }
    }

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
    def check(txt: List[A]): Boolean = {
      //println(s"txt: $txt\nthis: $this")
      (txt, this.children, this.content) match {
        case (_,       Nil, None) => false
        case (_,       _,   None) => this.children.exists(_.check(txt)) // all content now has Some
        case (Nil,     _,   _) => false  // all txt now has head
        case (tx::txs, Nil, Some(c)) => tx == c
        case (tx::txs, _, Some(c)) => (tx == c) && this.children.exists(_.check(txs))
      }
    }

    def search(txt: List[A]): List[Int] = {
//      def sch(tup: (List[A], Int)): (Int, Boolean) = {
//        val (txt, i) = tup
//        (i, this.check(txt))
//      }
//      txt.tails.zipWithIndex.map(sch).filter(_._2).map(_._1).toList
      txt.tails.zipWithIndex.filter((tup) => this.check(tup._1)).map(_._2).toList
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
  def trieCheck[A](txt: List[A], pats: List[List[A]]): Boolean = {
    val tr = fromLists(pats)
    tr.check(txt)
  }

  def insertStrs(ss: List[String]): Trie[Char] = {
    ss.map(_.toList).foldLeft(Empty: Trie[Char])(_.insertList(_))
  }

  def fmtAdjLst(xs: List[Trip[Char]]) = {
    val f = (t: Trip[Char]) => s"${t._1}->${t._2}:${t._3}"
    xs.map(f).mkString("\n")
  }

  def runIOLines(f: Answerer, fn: String) = {
    import java.io._
    val lines = scala.io.Source.fromFile(fn).getLines().toList  //.mkString
    val res = f(lines)
    val fout = s"$fn.ans.txt"
    val file = new File(fout)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(res)
    bw.close()
    println(fout)
  }

  // Type Aux. funcs
  type Answerer = List[String] => String
  def Tr[A](a: A): Trie[A] = new Trie(Some(a), Nil)
  def fromLists[U](xs: List[List[U]]): Trie[U] = xs.foldLeft(Empty: Trie[U])((t: Trie[U], l: List[U]) => t.insertList(l))

  def Empty[A]: Trie[A] = new Trie(None, Nil)

  def showList[A](xs: List[A]): String = {
    xs match {
      case Nil => ""
      case ys => s"[${ys.map(_.toString).mkString(" ")}]"
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
  val p1: Answerer = lines => fmtAdjLst(adjLst(insertStrs(lines)))
  val p2: Answerer = {
    case txt::xs => fromLists(xs.map(_.toList)).search(txt.toList).mkString(" ")
    case _ => throw new Error("Not enough lines")
  }
  val ans = Array((p1, "src/data/dataset_294_4.txt")
//    , (p2, )
  )

  def main (args: Array[String]) {
//    runIOLines(p1, "src/data/TrieConstruction.txt")
    runIOLines(p2, "src/data/dataset_294_8.txt")
//    runIOLines(p2, "src/data/w1_2.txt")

    println("Done.")
  }

}