package collections.list

import scala.annotation.tailrec

sealed trait List [+A] {
  def size: Long
  def isEmpty: Boolean = {
    this match {
      case Nil => true
      case _:List[A] => false
    }
  }

  def head: A
  def last: A

  def headOpt: Option[A] = Some(head)
  def lastOpt: Option[A] = Some(last)
  def tail: List[A]
  def init: List[A]

  // should return index of given element
  def indexOf[B >: A](elem: B): Long = {
    @tailrec def index(list: List[A], e: B, i: Long): Long = {
      e match {
        case list.head => i
        case _ => if (i > this.size) -1 else index(this.tail, e, i+1)
      }
    }
    index(this, elem, 0)
  }

  // Returns first n elements of the list
  // if n > list size it will return the whole list
  def take (n: Int): List[A] = {
    n match {
      case i if i > size => this
      case i if i < 1 => Nil
      case i if i == 1 => List(head)
      case i => tail.take(i-1).cons(head)
    }
  }

  // returns last n elements of the list
  def takeRight(n: Int): List[A] = {
    n match {
      case i if i > size => this
      case i if i < 1 => Nil
      case i if i == 1 => List(last)
      case i => init.takeRight(i-1).append(List(last))
    }
  }

  def reverse: List[A] = {
    init.reverse.cons(last)
  }

  def zip [B] (that: List[B]): List[(A, B)] = {}
  def zipWithIndex: List[(A, Long)] = {}

  def append[B >: A](that: List[B]): List[B]

  // appends element to the beginning of list
  def cons[B >: A](a: B): List[B]

  // High order functions
  // Recursively implement the following methods:
  def map [B] (f: (A) => B): List[B] = {
    List(f(head)).append(tail.map(f))
  }

  def filter (predicate: (A) => Boolean): List[A] = {
    tail match {
      case Nil => if (predicate(head)) List(head) else Nil
      case _: List[A] =>
        predicate(head) match {
          case true => tail.filter(predicate).cons(head)
          case false => tail.filter(predicate)
        }
    }
  }

  def count(predicate: (A) => Boolean): Int = {
    def counter(list: List[A], predicate: (A) => Boolean, acc: Long): Int = {
      list.tail match {
        case Nil => if (predicate(list.head)) 1 else 0
        case _: List[A] => counter(list.tail, predicate, acc + predicate(list.head))
      }
    }
    counter(this, predicate, 0)
  }
  def find (predicate: (A) => Boolean): Option[A] = {
    predicate(head) match {
      case true => Some(head)
      case false =>
        tail match {
          case Nil => None
          case _: List[A] => tail.find(predicate)}
    }
  }
  def exists(predicate: (A) => Boolean): Boolean = this.find(predicate).isDefined

  // Partial function
  // creates a new collection by its partial application to all elements
  // of given list where this function is defined
  def collect[B](pfun: PartialFunction[A,B]): List[B] = {
    tail.collect(pfun).cons(pfun(head))
  }

  // Currying
  def foldLeft [B] (z: B) (operator: (B, A) => B): B = {}
  def foldRight [B] (z: B) (operator: (A, B) => B): B = {}
}

object List {
  def apply[A] (items: A*): List[A] = {
    if (items.isEmpty) Nil
    else Cons.apply(items.head, apply(items.tail: _*), items.last, apply(items.init: _*))
  }

  def fill [A] (value: A)(size: Int): List[A] = {
    assert(size > 0)
    def items = for (_ <- 1 to size) yield value
    apply(items: _*)
  }

  def empty[A]: List[A] = Nil
}

case class Cons[+A](override val head: A, next: List[A], override val last: A, prev: List[A]) extends List[A] {
  override def headOpt: Option[A] = Some(head)
  override def lastOpt: Option[A] = Some(last)
  override def tail: List[A] = next
  override def init: List[A] = prev
  override def size: Long = next.size + 1
  override def cons[B >: A](a: B): List[B] = {
    Cons(a, tail.cons(head), last, init.cons(a))
  }
  override def append[B >: A](that: List[B]): List[B] = Cons(head, tail.append(that), that.last, init.append(that.init.cons(last)))
}

case object Nil extends List[Nothing] {
  def head: Nothing = _
  def last: Nothing = _
  def tail: List[Nothing] = _
  def init: List[Nothing] = _
  def size: Long = 0
  override def cons[B >: Nothing](a: B): List[B] = List(a)
  override def append[B >: Nothing](that: List[B]): List[B] = that
}


// something else is also missing ;)?