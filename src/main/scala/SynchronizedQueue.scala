import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.mutable.{ArrayBuffer, Seq}
import scala.collection.JavaConverters._
import scala.collection.TraversableOnce
import scala.reflect.ClassTag

class SynchronizedQueue[A : ClassTag] {
  private val underlying: ConcurrentLinkedQueue[A] = new ConcurrentLinkedQueue[A]()

  /** Checks if the queue is empty.
    *
    *  @return true, iff there is no element in the queue.
    */
  def isEmpty: Boolean = underlying.isEmpty

  def nonEmpty: Boolean = ! isEmpty

  /** Inserts a single element at the end of the queue.
    *
    *  @param  elem        the element to insert
    */
  def +=(elem: A): this.type = {
    underlying.add(elem)
    this
  }

  /** Adds all elements provided by a `TraversableOnce` object
    *  at the end of the queue. The elements are prepended in the order they
    *  are given out by the iterator.
    *
    *  @param  xs        a traversable object
    */
  def ++=(xs: TraversableOnce[A]): this.type = {
    for (x <- xs) { underlying.add(x) }
    this
  }

  /** Adds all elements to the queue.
    *
    *  @param  elems       the elements to add.
    */
  def enqueue(elems: A*): Unit = {
    for (elem <- elems) { underlying.add(elem) }
  }

  /** Returns the first element in the queue, and removes this element
    *  from the queue.
    *
    *  @throws java.util.NoSuchElementException
    *  @return the first element of the queue.
    */
  def dequeue(): A =
    if (isEmpty)
      throw new NoSuchElementException("queue empty")
    else {
      underlying.poll()
    }

  /** Returns the first element in the queue which satisfies the
    *  given predicate, and removes this element from the queue.
    *
    *  @param p   the predicate used for choosing the first element
    *  @return the first element of the queue for which p yields true
    */
  def dequeueFirst(p: A => Boolean): Option[A] =
    if (isEmpty)
      None
    else {
      underlying.iterator().asScala.toStream.find(p)
    }

  /** Returns all elements in the queue which satisfy the
    *  given predicate, and removes those elements from the queue.
    *
    *  @param p   the predicate used for choosing elements
    *  @return    a sequence of all elements in the queue for which
    *             p yields true.
    */
  def dequeueAll(p: A => Boolean): Seq[A] = {
    if (isEmpty)
      Seq.empty
    else {
      val res = new ArrayBuffer[A]
      underlying.iterator().asScala.foreach { v =>
        if (p(v)) res += v
      }
      res.foreach { v =>
        underlying.remove(v)
      }
      res
    }
  }

  /** Returns the first element in the queue, or throws an error if there
    *  is no element contained in the queue.
    *
    *  @return the first element.
    */
  def front: A = underlying.element()

  /** Removes all elements from the queue. After this operation is completed,
    *  the queue will be empty.
    */
  def clear(): Unit = underlying.clear()

  def contains(what: A): Boolean = underlying.contains(what)

  // TODO - Don't override this just for new to create appropriate type....
  def tail: SynchronizedQueue[A] = {
    val tl = new SynchronizedQueue[A]
    if ( ! isEmpty ) {
      tl.enqueue( underlying.iterator().asScala.drop(1).toArray :_* )
    } else {
      throw new Exception("Tail of an empty queue")
    }
    tl
  }

  /** Checks if two queues are structurally identical.
    *
    *  @return true, iff both queues contain the same sequence of elements.
    */
  override def equals(that_ : Any): Boolean = {
    def sameElements(q1: ConcurrentLinkedQueue[_], q2: ConcurrentLinkedQueue[_]): Boolean = {
      if (q1.size() == q2.size()) {
        val (q1It, q2It) = (q1.iterator(), q2.iterator())
        var result = true
        while(result && q1It.hasNext && q2It.hasNext) {
          result = q1It.next() == q2It.next()
        }
        result
      } else false
    }
    that_ match {
      case that: SynchronizedQueue[_] => (that eq this) || sameElements(this.underlying, that.underlying)
      case that: ConcurrentLinkedQueue[_] => sameElements(this.underlying, that)
      case _ => false
    }
  }

  /** Returns a textual representation of a queue as a string.
    *
    *  @return the string representation of this queue.
    */
  override def toString() = underlying.toString

  override def clone(): SynchronizedQueue[A] = {
    val cl = new SynchronizedQueue[A]
    cl.enqueue( underlying.iterator().asScala.toArray :_* )
    cl
  }
}

object SynchronizedQueue {

  def empty[A : ClassTag] = new SynchronizedQueue[A]

  def apply[A : ClassTag](elems: A*): SynchronizedQueue[A] = {
    if (elems.isEmpty) empty[A]
    else {
      val sq = empty[A]
      sq ++= elems
      sq
    }
  }
}
