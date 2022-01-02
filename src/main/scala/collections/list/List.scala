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
  def headOpt: Option[A]
  def lastOpt: Option[A]
  def tail: List[A]
  def init: List[A]

  // should return index of given element
  def indexOf[B >: A](elem: B): Long = {
    head match {
      case `elem` => 0
      case _ => tail match {
        case Nil => -1
        case _ => tail.indexOf(elem) + 1
      }
    }
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

  def zip [B] (that: List[B]): List[(A, B)] = {
    if (size != that.size) throw new UnsupportedOperationException("zipping lists of different size")
    tail match {
      case Nil => List((head, that.head))
      case _ => tail.zip(that.tail).cons((head, that.head))
    }
  }
  def zipWithIndex: List[(A, Long)] = {
    zip(List(1L to size: _*))
  }

  def append[B >: A](that: List[B]): List[B]

  // appends element to the beginning of list
  def cons[B >: A](a: B): List[B]

  def foreach[U](f: A => U): Unit = ???
  // High order functions
  // Recursively implement the following methods:
  def map [B] (f: A => B): List[B] = {
    tail match {
      case Nil => List(f(head))
      case _ => tail.map(f).cons(f(head))
    }
  }

  def filter (predicate: A => Boolean): List[A] = {
    tail match {
      case Nil => if (predicate(head)) List(head) else Nil
      case _ =>
        if (predicate(head)) tail.filter(predicate).cons(head)
        else tail.filter(predicate)
    }
  }

  def count(predicate: A => Boolean): Int = {
    @tailrec
    def counter(list: List[A], predicate: A => Boolean, acc: Long): Int = {
      list.tail match {
        case Nil => if (predicate(list.head)) 1 else 0
        case _ => counter(list.tail, predicate, if (predicate(list.head)) acc + 1 else acc)
      }
    }
    counter(this, predicate, 0)
  }
  def find(predicate: A => Boolean): Option[A] = {
    if (predicate(head)) Some(head)
    else tail match {
        case Nil => None
        case _: List[A] => tail.find(predicate)
      }
  }
  def exists(predicate: A => Boolean): Boolean = find(predicate).isDefined
  def forAll(predicate: A => Boolean): Boolean = {
    tail match {
      case Nil => predicate(head)
      case _ => if (predicate(head)) tail forAll predicate else false
    }
  }

  // Partial function
  // creates a new collection by its partial application to all elements
  // of given list where this function is defined
  def collect[B](pfun: PartialFunction[A,B]): List[B] = {
    tail.collect(pfun).cons(pfun(head))
  }

  // Currying
  def foldLeft [B] (z: B) (operator: (B, A) => B): B = {
    tail match {
      case Nil => operator(z, head)
      case _ => tail.foldLeft(z)(operator)
    }
  }
  def foldRight [B] (z: B) (operator: (A, B) => B): B = {
    init match {
      case Nil => operator(last, z)
      case _ => init.foldRight(z)(operator)
    }
  }

  def flatMap[B](f: A => IterableOnce[B]): List[B] = {
    tail match {
      case Nil => List(f(head).iterator.toSeq: _*)
      case _ => List(f(head).iterator.toSeq: _*).append(tail.flatMap(f))
    }
  }

  def withFilter(predicate: A => Boolean): WithFilter = new WithFilter(this, predicate)
  final class WithFilter (list: List[A], predicate: A => Boolean) {
    private val filtered = list.filter(predicate)
    def map[B](f: A => B): List[B] = filtered.map(f)

    def flatMap[B](f: A => IterableOnce[B]): List[B] = filtered.flatMap(f)

    def foreach[U](f: A => U): Unit = filtered.foreach(f)

    def withFilter(q: A => Boolean): WithFilter = new WithFilter(filtered, q)
  }
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

final case class Cons[+A](override val head: A, next: List[A], override val last: A, prev: List[A]) extends List[A] {
  def headOpt: Option[A] = Some(head)
  def lastOpt: Option[A] = Some(last)
  def tail: List[A] = next
  def init: List[A] = prev
  override def size: Long = next.size + 1
  override def cons[B >: A](a: B): List[B] = {
    Cons(a, tail.cons(head), last, init.cons(a))
  }
  override def append[B >: A](that: List[B]): List[B] = {
    Cons(head, tail.append(that), that.last, init.append(that.init.cons(last)))
  }
}

case object Nil extends List[Nothing] {
  def head: Nothing = throw new NoSuchElementException("head of empty list")
  def last: Nothing = throw new NoSuchElementException("last of empty list")
  def tail: List[Nothing] = throw new UnsupportedOperationException("tail of empty list")
  def init: List[Nothing] = throw new UnsupportedOperationException("init of empty list")
  def headOpt: Option[Nothing] = None
  def lastOpt: Option[Nothing] = None
  def size: Long = 0
  override def cons[B >: Nothing](a: B): List[B] = List(a)
  override def append[B >: Nothing](that: List[B]): List[B] = that
}


// something else is also missing ;)?
// isn't it about WithFilter class?
