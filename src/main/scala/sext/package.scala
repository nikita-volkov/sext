package sext

import util.Try
import reflect.runtime.universe._
import reflect.runtime.currentMirror
import collection.GenTraversableOnce

object `package` {

  implicit class MapExtensions [ K, V ] ( val x : Map[ K, V ] ) extends AnyVal {
    def filterValues(predicate: V => Boolean) =
      x.filter(pair => predicate(pair._2))
    def withValuesFilter(predicate: V => Boolean) =
      x.withFilter(pair => predicate(pair._2))
    def mapKeys[K2](f: K => K2) =
      x.map(pair => f(pair._1) -> pair._2)
  }

  implicit class TraversableExtensions
    [ ItemT,
      TraversableT[ItemT] <: Traversable[ItemT] ]
    ( traversable : TraversableT[ItemT] )
    {
      import collection.generic.CanBuildFrom

      def zipBy
        [ ResultItemT,
          ResultT ]
        ( implicit bf : CanBuildFrom[TraversableT[ItemT], (ItemT, ResultItemT), ResultT] )
        ( f : ItemT => ResultItemT )
        : ResultT
        = traversable.map(x ⇒ x → f(x)).asInstanceOf[ResultT]
    }

  implicit class AnyExtensions[A](x: A) {
    def tap[ResultT](f: A => ResultT) = { f(x); x }

    def as[ResultT](f: A => ResultT) = f(x)

    def isEmpty
      = x match {
        case null | () => true
        case x: Boolean => !x
        case x: Byte => x == 0.toByte
        case x: Short => x == 0.toShort
        case x: Char => x == 0.toChar
        case x: Int => x == 0
        case x: Long => x == 0l
        case x: Float => x == 0f
        case x: Double => x == 0d
        case x: Product => x.productArity == 0
        case x: GenTraversableOnce[_] => x.isEmpty
        case _ => false
      }

    def notNull = Option(x)

    def notEmpty = if (x.isEmpty) None else Some(x)

    def satisfying(p: A => Boolean) : Option[A]
      = if (p(x)) Some(x) else None

    def satisfying1
      ( p : A => Boolean )
      : Either[A, A]
      = if( p(x) ) Left(x)
        else Right(x)

    def trace [ B ] ( f : A => B = (x : A) => x.treeString )
      = { Console.println(f(x)); x }

    def trying
      [ ResultT ]
      ( f : A => ResultT )
      = Try(f(x)).toOption

  }

  implicit class StringExtensions
    ( s : String )
    {
      def notEmpty
        = if( s.isEmpty ) None else Some(s)
      def indent
        ( i : Int )
        = prependLines(" " * i)
      def prependLines
        ( p : String )
        = s.lines
            .reduceOption{ _ + "\n" + p + _ }
            .map{ p + _ }
            .getOrElse( p )
      def splitBy
        ( splitter : String )
        : (String, String)
        = s.indexOf(splitter) match {
            case -1 => (s, "")
            case i =>
              val (a, b) = s.splitAt(i)
              (a, b.drop(splitter.size))
          }
    }
  implicit class AnyInstanceOf[ A : TypeTag ]( x : A ) {
    def toInstanceOf[ T : TypeTag ] : Option[T]
      = {
        def test
          = currentMirror.runtimeClass(typeOf[T]) match {
              case c if c.isPrimitive => typeOf[A] <:< typeOf[T]
              case c => c.isAssignableFrom(x.getClass) || typeOf[A] <:< typeOf[T]
            }
        if( test ) Some( x.asInstanceOf[T] )
        else None
      }
  }
  implicit class AnyTreeString[ A ]( a : A ) {

    private def indent ( s : String )
      = s.lines.toStream match {
          case h +: t =>
            ( ("- " + h) +: t.map{"| " + _} ) mkString "\n"
          case _ => "- "
        }

    def treeString
      : String
      = a match {
          case x : Traversable[_] =>
            x.stringPrefix + ":\n" +
            x.view
              .map{ _.treeString }
              .map{ indent }
              .mkString("\n")
          case x : Product if x.productArity == 0 =>
            x.productPrefix
          case x : Product =>
            x.productPrefix + ":\n" +
            x.productIterator
              .map{ _.treeString }
              .map{ indent }
              .mkString("\n")
          case null =>
            "null"
          case _ =>
            a.toString
        }

    def valueTreeString
      : String
      = a match {
          case (k, v) =>
            k.valueTreeString + ":\n" +
            v.valueTreeString
          case a : TraversableOnce[_] =>
            a.toStream
              .map(_.valueTreeString)
              .map(indent)
              .mkString("\n")
          case a : Product =>
            currentMirror.reflect(a).symbol.typeSignature.members.toStream
              .collect{ case a : TermSymbol => a }
              .filterNot(_.isMethod)
              .filterNot(_.isModule)
              .filterNot(_.isClass)
              .map( currentMirror.reflect(a).reflectField )
              .map( f => f.symbol.name.toString.trim -> f.get )
              .reverse
              .as(collection.immutable.ListMap(_:_*))
              .valueTreeString
          case null =>
            "null"
          case _ =>
            a.toString
        }

  }
  implicit class AnyFunctional[ A ]( val α : A ) extends AnyVal {

    def unfold
      [ B ]
      ( ƒ : A => Option[(B, A)] )
      : Stream[B]
      = ƒ(α) map {case (β, α) ⇒ β #:: (α unfold ƒ)} getOrElse Stream()    

    def unfold1
      ( ƒ : A => Option[A] )
      : Stream[A]
      = ƒ(α) map (α ⇒ α #:: (α unfold1 ƒ)) getOrElse Stream()

    def iterate
      ( ƒ : A => A )
      : Stream[A]
      = α #:: (ƒ(α) iterate ƒ)

    def foldTo
      [ B ]
      ( σ : Traversable[B] )
      ( ƒ : (B, A) ⇒ A)
      = (σ foldRight α)(ƒ)

    def foldFrom
      [ B ]
      ( σ : Traversable[B] )
      ( ƒ : (A, B) ⇒ A)
      = (σ foldLeft α)(ƒ)

  }

  implicit class Any$ [A] ( val a : A ) extends AnyVal {
    @inline def $ [ResultT] ( f : A => ResultT ) = f(a)
  }
  implicit class Tuple2$$ [A, B] ( val a : (A, B) ) extends AnyVal {
    @inline def $$ [Z] ( f : (A, B) => Z ) = f.tupled(a)
  }
  implicit class Tuple3$$ [A, B, C] ( val a : (A, B, C) ) extends AnyVal {
    @inline def $$ [Z] ( f : (A, B, C) => Z ) = f.tupled(a)
  }

  /**
   * Useful for wrapping the function and passing as lambda when partially applied
   */
  def trying[A, B] ( f : A => B )( a : A ) = a trying f

  def memo [ X, R ] ( f : X => R ) = {
     // a WeakHashMap will release cache members if memory tightens
     val cache = new collection.mutable.WeakHashMap[X, R]
     x : X => cache.getOrElseUpdate( x, f(x) )
  }

}
