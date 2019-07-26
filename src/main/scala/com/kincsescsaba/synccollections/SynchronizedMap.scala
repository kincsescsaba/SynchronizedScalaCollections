package com.kincsescsaba.synccollections

import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters._
import scala.collection.Iterator
import scala.reflect.ClassTag

class SynchronizedMap[A, B] {
  private val underlying: ConcurrentHashMap[A, B] = new ConcurrentHashMap[A, B]()

  def get(key: A): Option[B] = Option(underlying.get(key))
  def iterator: Iterator[(A, B)] = underlying.entrySet().iterator().asScala.map(kv => kv.getKey -> kv.getValue)
  def += (kv: (A, B)): this.type = {
    underlying.put(kv._1, kv._2)
    this
  }
  def -= (key: A): this.type = {
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
  def update(key: A, value: B): Unit = { underlying.put(key, value) }
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
    this.iterator.foreach { kv =>
      update(kv._1, f(kv._1, kv._2))
    }
    this
  }
  def retain(p: (A, B) => Boolean): this.type = {
    this.iterator.foreach { kv =>
      if ( ! p(kv._1, kv._2) ) {
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
    underlying.entrySet().iterator().asScala.foreach { kv =>
      f(kv.getKey, kv.getValue)
    }
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

}

object SynchronizedMap {

  def empty[A : ClassTag, B : ClassTag] = new SynchronizedMap[A, B]

  def apply[A : ClassTag, B : ClassTag](elems: (A,B)*): SynchronizedMap[A, B] = {
    if (elems.isEmpty) empty[A, B]
    else {
      val sq = empty[A, B]
      sq ++= elems
      sq
    }
  }
}
