package geny

import scala.collection.mutable
import scala.reflect.ClassTag

/** Provides the `geny.Gen` data type, A Generator of elements of type [[A]].
 *
 *  [[Generator]] is basically the inverse of
 *  a `scala.Iterator`: instead of the core functionality being the pull-based
 *  `hasNext` and `next: T` methods, the core is based around the push-based
 *  `generate` method. `generate` is basically an extra-customizable version of
 *  `foreach`, which allows the person calling it to provide basic control-flow
 *  instructions to the upstream Gens.
 *
 *  Unlike a `scala.Iterator`, subclasses of [[Generator]] can guarantee any clean
 *  up logic is performed by placing it after the `generate` call is made.
 *
 *  Transformations on a [[Generator]] are lazy: calling methods like `filter`
 *  or `map` do not evaluate the entire Gen, but instead construct a new
 *  Gen that delegates to the original. The only methods that evaluate
 *  the [[Generator]] are the "Action" methods like
 *  `generate`/`foreach`/`find`, or the "Conversion" methods like `toArray` or
 *  similar.
 *
 *  `generate` takes a function returning `Gen.Action` rather that
 *  `Unit`. This allows a downstream Gen to provide basic control
 *  commands to the upstream Gens: i.e. [[Generator.End]] to cease
 *  enumeration of the upstream Gen. This allows it to avoid traversing and
 *  processing elements that the downstream Gen doesn't want/need to see
 *  anyway.
 *
 *  @tparam A TODO FILL IN
 */
trait Generator[+A]{
  /**
    * The core abstract method that defines the [[Generator]] trait. It is
    * essentially the same as `.foreach`, but with additional configurability.
    *
    * @param handleItem How to handle a single item: performs any desired side
    *                   effects, and returns a [[Generator.Action]] that
    *                   determines how to continue the enumeration.
    * @return an integer stating how many skipped elements from the
    *         `startingSkipped` input remain to be skipped after this
    *         `generate` call has completed.
    */
  def generate(handleItem: A => Generator.Action): Generator.Action


  // Actions
  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   */
  def foreach(f: A => Unit): Unit = generate{ x =>
    f(x)
    Generator.Continue
  }

  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   *  @return TODO FILL IN
   */
  def find(f: A => Boolean): Option[A] = {
    var result: Option[A] = None
    generate{ t =>
      if (!f(t)) Generator.Continue
      else{
        result = Some(t)
        Generator.End
      }
    }
    result
  }
  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   */
  def exists(f: A => Boolean) = find(f(_)).isDefined
  /** TODO FILL IN
   *
   *  @param a TODO FILL IN
   */
  def contains(a: Any) = exists(_ == a)
  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   */
  def forall(f: A => Boolean) = !exists(!f(_))
  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   */
  def count(f: A => Boolean = ((_: Any) => true)) = {
    var result = 0
    generate{ t =>
      if (f(t)) result +=  1
      Generator.Continue
    }
    result
  }
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param start TODO FILL IN
   *  @param f TODO FILL IN
   *  @return TODO FILL IN
   */
  def fold[B](start: B)(f: (B, A) => B): B = foldLeft(start)(f)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param start TODO FILL IN
   *  @param f TODO FILL IN
   *  @return TODO FILL IN
   */
  def foldLeft[B](start: B)(f: (B, A) => B): B = {
    var result = start
    generate{ t =>
      result = f(result, t)
      Generator.Continue
    }
    result
  }


  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param f TODO FILL IN
   *  @return TODO FILL IN
   */
  def reduce[B >: A](f: (B, A) => B): B = reduceLeft(f)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param f TODO FILL IN
   *  @return TODO FILL IN
   */
  def reduceLeft[B >: A](f: (B, A) => B): B = {
    var result: Option[B] = None
    generate{ t =>
      result = result match{
        case None => Some(t)
        case Some(old) => Some(f(old, t))
      }
      Generator.Continue
    }
    result.getOrElse(
      throw new UnsupportedOperationException("empty.reduceLeft")
    )
  }

  // Builders
  /** TODO FILL IN
   *
   *  @param pred TODO FILL IN
   *  @return TODO FILL IN
   */
  def filter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** TODO FILL IN
   *
   *  @param pred TODO FILL IN
   *  @return TODO FILL IN
   */
  def withFilter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param func TODO FILL IN
   *  @return TODO FILL IN
   */
  def map[B](func: A => B): Generator[B] = new Generator.Mapped[B, A](this, func)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param func TODO FILL IN
   *  @return TODO FILL IN
   */
  def flatMap[B](func: A => Generator[B]): Generator[B] = new Generator.FlatMapped[B, A](this, func)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param func TODO FILL IN
   *  @return TODO FILL IN
   */
  def collect[B](func: PartialFunction[A, B]): Generator[B] =
    filter(func.isDefinedAt).map(func)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param func TODO FILL IN
   *  @return TODO FILL IN
   */
  def collectFirst[B](func: PartialFunction[A, B]): Option[B] =
    filter(func.isDefinedAt).map(func).headOption

  /** TODO FILL IN
   *
   *  @tparam V TODO FILL IN
   *  @param f TODO FILL IN
   */
  def flatten[V](implicit f: A => Generator[V]) = this.flatMap[V]((x: A) => f(x))
  /** TODO FILL IN
   *
   *  @param start TODO FILL IN
   *  @param end TODO FILL IN
   *  @return TODO FILL IN
   */
  def slice(start: Int, end: Int): Generator[A] = new Generator.Sliced(this, start, end)
  /** TODO FILL IN
   *
   *  @param size TODO FILL IN
   *  @return TODO FILL IN
   */
  def grouped(size: Int): Generator[Iterable[A]] = sliding(size, step = size)
  /** TODO FILL IN
   *
   *  @param size TODO FILL IN
   *  @param step TODO FILL IN
   *  @return TODO FILL IN
   */
  def sliding(size: Int, step: Int): Generator[Iterable[A]] = {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")
    new Generator.Sliding(this, size, step)
  }
  /** TODO FILL IN
   *
   *  @param n TODO FILL IN
   */
  def take(n: Int) = slice(0, n)
  /** TODO FILL IN
   *
   *  @param n TODO FILL IN
   */
  def drop(n: Int) = slice(n, Int.MaxValue)
  /** TODO FILL IN
   *
   *  @param pred TODO FILL IN
   *  @return TODO FILL IN
   */
  def takeWhile(pred: A => Boolean): Generator[A] = new Generator.TakeWhile(this, pred)
  /** TODO FILL IN
   *
   *  @param pred TODO FILL IN
   *  @return TODO FILL IN
   */
  def dropWhile(pred: A => Boolean): Generator[A] = new Generator.DropWhile(this, pred)
  /** TODO FILL IN */
  def zipWithIndex: Generator[(A, Int)] = new Generator.ZipWithIndexed(this)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param other TODO FILL IN
   *  @return TODO FILL IN
   */
  def zip[B](other: Iterable[B]): Generator[(A, B)] = new Generator.Zipped(this, other)
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param other TODO FILL IN
   *  @return TODO FILL IN
   */
  def ++[B >: A](other: Generator[B]): Generator[B] = new Generator.Concat(this, other)

  // Conversions
  /** TODO FILL IN */
  def head = take(1).toSeq.head
  /** TODO FILL IN */
  def headOption = take(1).toSeq.headOption
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @return TODO FILL IN
   */
  def toBuffer[B >: A]: mutable.Buffer[B] = {
    val arr = mutable.Buffer.empty[B]
    foreach{arr.append(_)}
    arr

  }
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @return TODO FILL IN
   */
  def toArray[B >: A : ClassTag]: Array[B] = toBuffer.toArray
  /** TODO FILL IN */
  def toSeq: Seq[A] = toVector
  /** TODO FILL IN */
  def toList = toBuffer.toList
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   */
  def toSet[B >: A] = toBuffer[B].toSet
  /** TODO FILL IN */
  def toVector = toBuffer.toVector
  /** TODO FILL IN
   *
   *  @param start TODO FILL IN
   *  @param sep TODO FILL IN
   *  @param end TODO FILL IN
   *  @return TODO FILL IN
   */
  def mkString(start: String, sep: String, end: String): String = {
    val sb = new StringBuilder
    sb.append(start)
    var first = true
    foreach { x =>
      if (!first) {
        sb.append(sep)
      }
      sb.append(x)
      first = false
    }
    sb.append(end)
    sb.toString()
  }
  /** TODO FILL IN
   *
   *  @param sep TODO FILL IN
   *  @return TODO FILL IN
   */
  def mkString(sep: String): String = mkString("", sep, "")
  /** TODO FILL IN */
  def mkString: String = mkString("")


  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param num TODO FILL IN
   *  @return TODO FILL IN
   */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param num TODO FILL IN
   *  @return TODO FILL IN
   */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param cmp TODO FILL IN
   *  @return TODO FILL IN
   */
  def min[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param cmp TODO FILL IN
   *  @return TODO FILL IN
   */
  def max[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }

  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param f TODO FILL IN
   *  @param cmp TODO FILL IN
   *  @return TODO FILL IN
   */
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    var maxF: B = null.asInstanceOf[B]
    var maxElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- this) {
      val fx = f(elem)
      if (first || cmp.gt(fx, maxF)) {
        maxElem = elem
        maxF = fx
        first = false
      }
    }
    maxElem
  }
  /** TODO FILL IN
   *
   *  @tparam B TODO FILL IN
   *  @param f TODO FILL IN
   *  @param cmp TODO FILL IN
   *  @return TODO FILL IN
   */
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    var minF: B = null.asInstanceOf[B]
    var minElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- this) {
      val fx = f(elem)
      if (first || cmp.lt(fx, minF)) {
        minElem = elem
        minF = fx
        first = false
      }
    }
    minElem
  }

}

object Generator{
  /** TODO FILL IN */
  sealed trait Action
  object End extends Action
  object Continue extends Action


  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   *  @param xs TODO FILL IN
   */
  def apply[T](xs: T*) = from(xs)

  /** TODO FILL IN
   *
   *  @tparam M TODO FILL IN
   *  @tparam T TODO FILL IN
   *  @param t TODO FILL IN
   *  @param convert TODO FILL IN
   *  @return TODO FILL IN
   */
  implicit def from[M[_], T](t: M[T])(implicit convert: (M[T] => TraversableOnce[T])): Generator[T] = new Generator[T]{
    def generate(f: T => Generator.Action) = {
      var last: Generator.Action = Generator.Continue
      val iterator = convert(t).toIterator

      while (last == Generator.Continue && iterator.hasNext) {
        last = f(iterator.next())
      }
      last
    }
    override def toString = s"Generator($t)"
  }


  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   *  @param makeIterator TODO FILL IN
   *  @return TODO FILL IN
   */
  def selfClosing[T](makeIterator: => (Iterator[T], () => Unit)): Generator[T] = new SelfClosing(makeIterator)

  private class SelfClosing[+T](makeIterator: => (Iterator[T], () => Unit)) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      var last: Generator.Action = Generator.Continue
      val (iterator, onComplete) = makeIterator
      try {
        while (last == Generator.Continue && iterator.hasNext) {
          last = f(iterator.next())
        }
        last
      } finally{
        onComplete()
      }
    }
    /** TODO FILL IN */
    override def toString = s"Gen.SelfClosing(...)"
  }



  private class Concat[+T](inner: Generator[T], other: Generator[T]) extends Generator[T] {
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      val res1 = inner.generate(f)
      if (res1 == Generator.End) Generator.End
      else other.generate(f)

    }
    /** TODO FILL IN */
    override def toString = s"$inner ++ $other"
  }

  private class ZipWithIndexed[+T](inner: Generator[T]) extends Generator[(T, Int)] {
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: ((T, Int)) => Generator.Action) = {
      var i = 0
      inner.generate{t =>
        val res = f(t, i)
        i += 1
        res
      }
    }
    /** TODO FILL IN */
    override def toString = s"$inner.zipWithIndex"
  }

  private class Zipped[+T, V](inner: Generator[T], other: Iterable[V]) extends Generator[(T, V)] {
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: ((T, V)) => Generator.Action) = {
      val iter = other.iterator
      inner.generate{t =>
        if (!iter.hasNext) Generator.End
        else f(t, iter.next())
      }
    }
    /** TODO FILL IN */
    override def toString = s"$inner.zip($other)"
  }

  private class Filtered[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => if (pred(t)) f(t) else Generator.Continue}
    }
    /** TODO FILL IN */
    override def toString = s"$inner.filter($pred)"
  }

  private class Mapped[+T, V](inner: Generator[V], func: V => T) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => f(func(t))}
    }
    /** TODO FILL IN */
    override def toString = s"$inner.map($func)"
  }

  private class FlatMapped[+T, V](inner: Generator[V], func: V => Generator[T]) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {

      inner.generate{ outerT =>
        func(outerT).generate{ innerT =>
          f(innerT)
        }
      }
    }
    /** TODO FILL IN */
    override def toString = s"$inner.map($func)"
  }

  private class Sliding[+T](inner: Generator[T], size: Int, step: Int) extends Generator[Iterable[T]] {
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: Iterable[T] => Generator.Action) = {
      val builder = Iterable.newBuilder[T]
      val capacity = Math.max(size, step)
      var n = 0
      builder.sizeHint(capacity)
      var action: Generator.Action = Generator.Continue
      inner.generate {
        t =>
          if (n < capacity) {
            builder += t
            n += 1
            action
          }
          else {
            action = f(builder.result.take(size))
            val remainder = builder.result.drop(step)
            builder.clear
            builder.sizeHint(capacity)
            builder ++= remainder
            builder += t
            n = remainder.size + 1
            action
          }
      }
      if (n > 0 && action == Generator.Continue) {
        f(builder.result.take(size))
      }
      else action
    }
  }

  private class Sliced[+T](inner: Generator[T], start: Int, end: Int) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      var count = 0

      inner.generate{t =>
        if (count < start){
          count += 1
          Generator.Continue
        }else if (count < end){
          count += 1
          if (count != end) f(t)
          else {
            // If we've reached the limit of our slice, evaluate `f` once but
            // then `End` immediately. This saves us having to consume one more
            // item from `inner`, only to ignore/discard it when we realize
            // we're done.
            f(t)
            Generator.End
          }
        } else Generator.End
      }
    }
    /** TODO FILL IN */
    override def toString = s"$inner.slice($start, $end)"
  }

  private class TakeWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t =>
        if (pred(t)) {
          f(t)
        } else {
          Generator.End
        }
      }
    }
    /** TODO FILL IN */
    override def toString = s"$inner.takeWhile($pred)"
  }

  private class DropWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** TODO FILL IN
     *
     *  @param f TODO FILL IN
     */
    def generate(f: T => Generator.Action) = {
      var started = false
      inner.generate{t =>
        if (!started) {
          if (pred(t)) Generator.Continue
          else {
            started = true
            f(t)
          }
        }else f(t)
      }
    }
    /** TODO FILL IN */
    override def toString = s"$inner.dropWhile($pred)"
  }

}
