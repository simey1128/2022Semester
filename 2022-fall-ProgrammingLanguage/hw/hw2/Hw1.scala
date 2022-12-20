sealed trait IntList
case object Nil extends IntList
case class Cons(v: Int, t: IntList) extends IntList

sealed trait BTree
case object Leaf extends BTree
case class IntNode(v: Int, left: BTree, right: BTree) extends BTree

sealed trait Formula
case object True extends Formula
case object False extends Formula
case class Not(f: Formula) extends Formula
case class Andalso(left: Formula, right: Formula) extends Formula
case class Orelse(left: Formula, right: Formula)  extends Formula
case class Implies(left: Formula, right: Formula) extends Formula

object Hw1_Method {
    def concat(fst: IntList, snd: IntList): IntList = {
      fst match {
        case Nil => snd
        case Cons(h,t) => Cons(h, concat(t, snd))
      }
    }

  def reverse(list: IntList): IntList = {
    list match {
      case Nil => list
      case Cons(h,t) => concat(reverse(t), Cons(h, Nil))
    }
  }

  def fold(init: Int, ftn:(Int, Int)=> Int, list: IntList): Int = {
    list match {
      case Nil => init
      case Cons(h,t) => fold(ftn(init, h), ftn, t)
    }
  }
}

object Hw1 extends App {
  
  def fibo(n: Int): Int = {
    if(n<2) 1
    else fibo(n-1) + fibo(n-2)
  } 

  def sum(f: Int=>Int, n: Int): Int = {
    if(n==1) f(1)
    else f(n) + sum(f,n-1)
  }

  def foldRight(init: Int, ftn: (Int, Int)=>Int, list: IntList): Int = {  
    Hw1_Method.fold(init, ftn, Hw1_Method.reverse(list))
  }

  def filter(f: Int => Boolean, list: IntList): IntList = {
    list match {
      case Nil => Nil
      case Cons(h,t) => {
        if(f(h)) Cons(h, filter(f,t))
        else filter(f,t)
      }
  }
  }

  def iter[A](f: A => A, n: Int): A => A = (x: A) => {
    if (n==1) f(x)
    else iter[A](f,n-1)(f(x))
  }
  
  def insert(t: BTree, a: Int): BTree = {
    t match {
      case Leaf => IntNode(a, Leaf, Leaf)
      case IntNode(v,left,right) => {
        if(a>v) IntNode(v, left, insert(right, a))
        else if(a<v) IntNode(v, insert(left,a), right)
        else IntNode(v,left,right)
      }
    }
  }

  def eval(f: Formula): Boolean = {
    f match {
      case True => true
      case False => false
      case Not(f) => !eval(f)
      case Andalso(left, right) => eval(left) && eval(right)
      case Orelse(left, right) => eval(left) || eval(right)
      case Implies(left, right) => {
        if(eval(left)==eval(right)) true
        else false
      }
    }
  }



}


