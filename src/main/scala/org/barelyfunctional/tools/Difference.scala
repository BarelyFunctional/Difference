package org.barelyfunctional.tools

object Difference {

  case class PathDifference(path : List[ContainerKey[_]] = List.empty, leaf : LeafDifference[_]) {
    def add(key : ContainerKey[_]) = copy(path = key :: path)
  }

  def differences(difference : Difference[_]) : List[PathDifference] =
    difference match {
      case ld : LeafDifference[_] => List(PathDifference(leaf = ld))
      case cd : ContainerDifference[_, _, _] => cd.differences.toList.flatMap {
        case (ck : ContainerKey[_], difference) => differences(difference).map(_.add(ck))
      }
    }

  def lhsOnly(difference : Difference[_]) : List[(List[ContainerKey[_]], AnyRef)] = {
    List.empty
  }

  def rhsOnly(difference : Difference[_]) : List[(List[ContainerKey[_]], AnyRef)] = {
    List.empty
  }

  def apply[T](lhs : T, rhs : T) : Difference[T] = {
    if (lhs == rhs)
       NoDifference(lhs)
    else if (lhs.getClass != rhs.getClass)
       ClassDifference(lhs.asInstanceOf[Object], rhs.asInstanceOf[Object]).asInstanceOf[Difference[T]]
    else
      ((lhs, rhs) match {
        case (l : String, r : String) => StringDifference(l, r)
        case (l : Number, r : Number) => NumberDifference(l, r)
        case (l : List[_], r : List[_]) => ListDifference(l, r)
        case (l : Map[_, _], r : Map[_, _]) => MapDifference(l, r)
        case (l : Set[_], r : Set[_]) => SetDifference(l, r)
        case (l : Product, r : Product) => CaseClassDifference(l, r)
        case _ => ObjectDifference(lhs, rhs)
      }).asInstanceOf[Difference[T]]
  }
}


trait Lhs[T] {
  def lhs : T
}

trait Rhs[T] {
  def rhs : T
}

trait Difference[T] extends Lhs[T] with Rhs[T]

trait LeafDifference[T] extends Difference[T]

trait ContainerKey[T] {
  def key : T
}

trait ContainerDifference[K <: ContainerKey, V, T] extends Difference[T] {
  def lhsOnly : Map[K, V]
  def rhsOnly : Map[K, V]
  def differences : Map[K, Difference[V]]
}

//case class LhsOnly[T](lhs : T) extends Lhs[T] with Difference
//
//case class RhsOnly[T](rhs : T) extends Rhs[T] with Difference

case class NoDifference[T](same : T) extends LeafDifference[T] {
  def lhs = same
  def rhs = same
}

case class MapKey[T](key : T) extends ContainerKey[T]

case class MapDifference[K, V](lhs : Map[K, V], rhs : Map[K, V]) extends ContainerDifference[MapKey[K], V, Map[K, V]] {

  def lhsOnly = (lhs -- rhs.keySet).map { case (k, v) => (MapKey(k), v) }.toMap
  def rhsOnly = (rhs -- lhs.keySet).map { case (k, v) => (MapKey(k), v) }.toMap

  def differences = lhs.keySet.intersect(rhs.keySet).flatMap {
    k =>

  }
}

case class SetDifference[T](lhs : Set[T], rhs : Set[T]) extends LeafDifference[Set[T]] {
  def lhsOnly = lhs -- rhs
  def rhsOnly = rhs -- lhs
}

case class CaseClassDifference(lhs : Product, rhs : Product) extends ContainerDifference[CaseClassDifference#Key, AnyRef, Product] {
  case class Key(key : String) extends ContainerKey[String]

  def lhsOnly = Map.empty
  def rhsOnly = Map.empty

  def differences = lhs.keySet.intersect(rhs.keySet).flatMap {
    k =>

  }

}

case class ListDifference[T](lhs : List[T], rhs : List[T]) extends ContainerDifference[ListDifference#Key, T, List[T]] {

  case class Key(key : Int) extends ContainerKey[Int]

  def minLength = Math.min(lhs.length, rhs.length)

  def lhsOnly = lhs -- rhs.keySet
  def rhsOnly = rhs -- lhs.keySet

  def differences = lhs.keySet.intersect(rhs.keySet).flatMap {
    k =>

  }
}

case class ObjectDifference[T](lhs : T, rhs : T) extends LeafDifference[T]

case class StringDifference(lhs : String, rhs : String) extends LeafDifference[String]

case class NumberDifference(lhs : Number, rhs : Number) extends LeafDifference[Number]

case class ClassDifference(lhs : Object, rhs : Object) extends LeafDifference[Object]