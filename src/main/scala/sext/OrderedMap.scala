package sext

import collection.immutable.Queue

object OrderedMap {
  def apply[A, B](elems: (A, B)*) =
    new OrderedMap(Map(elems: _*), Queue(elems.map(_._1): _*))
}
/**
 * An ordered map implementation that should perform effectively on all operations except
 * removal, where it performs linearly.
 */
class OrderedMap[A, B](
  map: Map[A, B],
  protected val order: Queue[A]
) extends Map[A, B] {
  def get(key: A) =
    map.get(key)
  def iterator =
    order.iterator.map(x => x -> map(x))
  def +[B1 >: B](kv: (A, B1)) =
    new OrderedMap(
      map + kv,
      if( map.contains(kv._1) ) order else order enqueue kv._1
    )
  def -(key: A) =
    new OrderedMap(
      map - key,
      order diff Queue(key)
    )
  override def hashCode() =
    order.hashCode
  override def equals(that: Any) =
    that match {
      case that: OrderedMap[A, B] =>
        order.equals(that.order)
      case _ =>
        super.equals(that)
    }
}
