package com.kincsescsaba.synccollections

import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters._
import scala.collection.Iterator
import scala.reflect.ClassTag
import scala.compat.java8.FunctionConverters._



class SynchronizedMap[A : ClassTag, B : ClassTag] private
        (underlying: ConcurrentHashMap[A, B] = new ConcurrentHashMap[A, B]()) {

  private val thisMap = this

  def get(key: A): Option[B] = Option(underlying.get(key))

  def iterator: Iterator[(A, B)] = underlying.entrySet().iterator().asScala.map(kv => kv.getKey -> kv.getValue)

  def +=(kv: (A, B)): this.type = {
    underlying.put(kv._1, kv._2)
    this
  }

  def -=(key: A): this.type = {
    underlying.remove(key)
    this
  }
  def ++=(xs: IterableOnce[(A, B)]): this.type = {
    for (x <- xs.iterator) { underlying.put(x._1, x._2) }
    this
  }

  def size: Int = underlying.size()

  def put(key: A, value: B): Option[B] = {
    Option(underlying.put(key, value))
  }

  def update(key: A, value: B): Unit = {
    underlying.put(key, value)
  }

  def remove(key: A): Option[B] = {
    Option(underlying.remove(key))
  }

  def clear(): Unit = underlying.clear()

  def getOrElseUpdate(key: A, default: => B): B = {
    get(key) match {
      case Some(v) => v
      case _ => {
        put(key, default)
        default
      }
    }
  }

  def transform(f: (A, B) => B): this.type = {
    for (kv <- this) {
      update(kv._1, f(kv._1, kv._2))
    }
    this
  }

  def retain(p: (A, B) => Boolean): this.type = {
    for (kv <- this) {
      if (!p(kv._1, kv._2)) {
        remove(kv._1)
      }
    }
    this
  }

  def values: scala.collection.Iterable[B] = {
    underlying.values().asScala
  }

  def valuesIterator: Iterator[B] = {
    underlying.values().iterator().asScala
  }

  def foreach[U](f: ((A, B)) => U) = {
    val returnsUnit = { (a: A, b: B) => {
      f(a -> b); ()
    }
    }
    underlying.forEach(asJavaBiConsumer(returnsUnit))
  }

  def map[T : ClassTag](f: ((A, B)) => T): SynchronizedMap[A, T] = {
    val mapped = new ConcurrentHashMap[A, T]()
    def doMap(kv: (A, B)) {
      mapped.put(kv._1, f(kv))
    }
    foreach { doMap }
    new SynchronizedMap[A, T](mapped)
  }

  def flatMap[K2, V2](f: ((A, B)) => IterableOnce[(K2, V2)]): SynchronizedMap[K2, V2] = {
    val mapped = new ConcurrentHashMap[K2, V2]()
    def doMap(kv: (A, B)) {
      f(kv).iterator foreach { r =>
          mapped.put(r._1, r._2)
      }
    }
    foreach { doMap }
    new SynchronizedMap[K2, V2](mapped)
  }

  def flatMap[T](f: ((A, B)) => IterableOnce[T]): Iterable[T] = {
    iterator.toSeq.flatMap(f)
  }

  def apply(key: A): B = {
    underlying.get(key)
  }

  def keySet: scala.collection.Set[A] = {
    underlying.keySet().asScala.toSet
  }

  def keys: scala.collection.Iterable[A] = {
    underlying.keys().asScala.toList
  }

  def keysIterator: Iterator[A] = {
    keys.iterator
  }

  def isEmpty: Boolean = {
    underlying.isEmpty
  }

  def contains(key: A): Boolean = {
    underlying.containsKey(key)
  }

  def isDefinedAt(key: A) = {
    contains(key)
  }

  override def toString() = underlying.toString

  override def clone(): SynchronizedMap[A, B] = {
    SynchronizedMap(iterator.toArray: _*)
  }

  /*def withFilter(p: (A, B) => Boolean): FilterMonadic[(A, B), SynchronizedMap[A, B]] = new WithFilter(p)

  class WithFilter(p: (A, B) => Boolean) extends FilterMonadic[(A, B), SynchronizedMap[A, B]] {
    def map[T, That](f: (A, B) => T): That = {

    }
    def flatMap[T, That](f: (A, B) => scala.collection.GenTraversableOnce[T]): That = {

    }
    def foreach[U](f: (A, B) => U): Unit = {

    }
    def withFilter(p: (A, B) => Boolean): FilterMonadic[(A, B), SynchronizedMap[A, B]] = {

    }
  }*/
}
object SynchronizedMap {

  def empty[A : ClassTag, B : ClassTag] = new SynchronizedMap[A, B]

  def apply[A : ClassTag, B : ClassTag](elems: (A, B)*): SynchronizedMap[A, B] = {
    if (elems.isEmpty) empty[A, B]
    else {
      val sq = empty[A, B]
      sq ++= elems
      sq
    }
  }
}
