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
 *  @tparam A the type of elements produced by this Generator
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
  /** Applies `f` to each element of this Generator, for its side effects.
   *
   *  @param f the function to apply to each element
   */
  def foreach(f: A => Unit): Unit = generate{ x =>
    f(x)
    Generator.Continue
  }

  /** Returns the first element satisfying `f`, stopping the enumeration once found.
   *
   *  @param f the predicate to test each element against
   *  @return an `Option` holding the first matching element, or `None`
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
  /** Tests whether some element satisfies `f`.
   *
   *  @param f the predicate to test each element against
   */
  def exists(f: A => Boolean) = find(f(_)).isDefined
  /** Tests whether this Generator contains the element `a`.
   *
   *  @param a the element to search for
   */
  def contains(a: Any) = exists(_ == a)
  /** Tests whether all elements satisfy `f`.
   *
   *  @param f the predicate to test each element against
   */
  def forall(f: A => Boolean) = !exists(!f(_))
  /** Counts the elements satisfying `f`, or all elements by default.
   *
   *  @param f the predicate to test each element against; defaults to accepting every element
   */
  def count(f: A => Boolean = ((_: Any) => true)) = {
    var result = 0
    generate{ t =>
      if (f(t)) result +=  1
      Generator.Continue
    }
    result
  }
  /** Folds the elements from left to right, starting with `start`. Equivalent to `foldLeft`.
   *
   *  @tparam B the type of the accumulated result
   *  @param start the initial value of the accumulator
   *  @param f the function combining the accumulator with each element
   *  @return the final accumulated value
   */
  def fold[B](start: B)(f: (B, A) => B): B = foldLeft(start)(f)
  /** Folds the elements from left to right, starting with `start`, combining them with `f`.
   *
   *  @tparam B the type of the accumulated result
   *  @param start the initial value of the accumulator
   *  @param f the function combining the accumulator with each element
   *  @return the final accumulated value
   */
  def foldLeft[B](start: B)(f: (B, A) => B): B = {
    var result = start
    generate{ t =>
      result = f(result, t)
      Generator.Continue
    }
    result
  }


  /** Reduces the elements from left to right using `f`. Equivalent to `reduceLeft`.
   *
   *  @tparam B the supertype of the elements, used as the reduction type
   *  @param f the function combining the accumulated value with each element
   *  @return the reduced value
   */
  def reduce[B >: A](f: (B, A) => B): B = reduceLeft(f)
  /** Reduces the elements from left to right using `f`.
   *
   *  @tparam B the supertype of the elements, used as the reduction type
   *  @param f the function combining the accumulated value with each element
   *  @return the reduced value
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
  /** Lazily returns a Generator containing only the elements satisfying `pred`.
   *
   *  @param pred the predicate an element must satisfy to be kept
   *  @return a Generator of the filtered elements
   */
  def filter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Lazily filters the elements by `pred`, for use in `for` comprehensions.
   *
   *  @param pred the predicate an element must satisfy to be kept
   *  @return a Generator of the filtered elements
   */
  def withFilter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Lazily transforms each element using `func`.
   *
   *  @tparam B the type of the transformed elements
   *  @param func the function applied to each element
   *  @return a Generator of the transformed elements
   */
  def map[B](func: A => B): Generator[B] = new Generator.Mapped[B, A](this, func)
  /** Lazily transforms each element into a Generator using `func`, and flattens the results.
   *
   *  @tparam B the type of elements in the resulting Generators
   *  @param func the function mapping each element to a Generator
   *  @return a Generator of the concatenated elements
   */
  def flatMap[B](func: A => Generator[B]): Generator[B] = new Generator.FlatMapped[B, A](this, func)
  /** Lazily applies the partial function `func` to the elements it is defined at, discarding the rest.
   *
   *  @tparam B the type of the transformed elements
   *  @param func the partial function applied to the elements it is defined at
   *  @return a Generator of the transformed elements
   */
  def collect[B](func: PartialFunction[A, B]): Generator[B] =
    filter(func.isDefinedAt).map(func)
  /** Returns the result of applying the partial function `func` to the first element it is defined at, if any.
   *
   *  @tparam B the type of the transformed element
   *  @param func the partial function applied to the elements it is defined at
   *  @return an `Option` holding the first transformed element, or `None`
   */
  def collectFirst[B](func: PartialFunction[A, B]): Option[B] =
    filter(func.isDefinedAt).map(func).headOption

  /** Flattens a Generator of elements that can each be viewed as a Generator.
   *
   *  @tparam V the type of elements in the inner Generators
   *  @param f the implicit conversion from an element to a Generator
   */
  def flatten[V](implicit f: A => Generator[V]) = this.flatMap[V]((x: A) => f(x))
  /** Lazily returns the elements from index `start` up to, but not including, index `end`.
   *
   *  @param start the index of the first element to include
   *  @param end the index after the last element to include
   */
  def slice(start: Int, end: Int): Generator[A] = new Generator.Sliced(this, start, end)
  /** Lazily groups the elements into Iterables of length `size`, the last possibly shorter.
   *
   *  @param size the number of elements in each group
   */
  def grouped(size: Int): Generator[Iterable[A]] = sliding(size, step = size)
  /** Lazily groups the elements into sliding windows of length `size`, advancing by `step` each time.
   *
   *  @param size the number of elements in each window
   *  @param step the number of elements to advance between windows
   *  @return a Generator of the windows
   */
  def sliding(size: Int, step: Int): Generator[Iterable[A]] = {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")
    new Generator.Sliding(this, size, step)
  }
  /** Lazily returns the first `n` elements.
   *
   *  @param n the number of elements to take
   */
  def take(n: Int) = slice(0, n)
  /** Lazily returns the elements after the first `n`.
   *
   *  @param n the number of elements to drop
   */
  def drop(n: Int) = slice(n, Int.MaxValue)
  /** Lazily returns the longest prefix of elements satisfying `pred`.
   *
   *  @param pred the predicate an element must satisfy to be taken
   *  @return a Generator of the taken elements
   */
  def takeWhile(pred: A => Boolean): Generator[A] = new Generator.TakeWhile(this, pred)
  /** Lazily drops the longest prefix of elements satisfying `pred`.
   *
   *  @param pred the predicate an element must satisfy to be dropped
   *  @return a Generator of the remaining elements
   */
  def dropWhile(pred: A => Boolean): Generator[A] = new Generator.DropWhile(this, pred)
  /** Pairs each element with its index, starting from zero. */
  def zipWithIndex: Generator[(A, Int)] = new Generator.ZipWithIndexed(this)
  /** Lazily pairs each element with the corresponding element of `other`, stopping at the shorter.
   *
   *  @tparam B the type of elements in `other`
   *  @param other the Iterable to zip with
   *  @return a Generator of the pairs
   */
  def zip[B](other: Iterable[B]): Generator[(A, B)] = new Generator.Zipped(this, other)
  /** Lazily concatenates this Generator with `other`.
   *
   *  @tparam B the supertype of the elements of both Generators
   *  @param other the Generator whose elements are appended
   *  @return a Generator of the elements of both
   */
  def ++[B >: A](other: Generator[B]): Generator[B] = new Generator.Concat(this, other)

  // Conversions
  /** Returns the first element, throwing if this Generator is empty. */
  def head = take(1).toSeq.head
  /** Returns the first element as an `Option`, or `None` if this Generator is empty. */
  def headOption = take(1).toSeq.headOption
  /** Collects the elements into a new `mutable.Buffer`.
   *
   *  @tparam B the type of elements in the Buffer
   *  @return a Buffer containing all the elements
   */
  def toBuffer[B >: A]: mutable.Buffer[B] = {
    val arr = mutable.Buffer.empty[B]
    foreach{arr.append(_)}
    arr

  }
  /** Collects the elements into a new `Array`.
   *
   *  @tparam B the type of elements in the Array
   *  @return an Array containing all the elements
   */
  def toArray[B >: A : ClassTag]: Array[B] = toBuffer.toArray
  /** Collects the elements into a `Seq`. */
  def toSeq: Seq[A] = toVector
  /** Collects the elements into a `List`. */
  def toList = toBuffer.toList
  /** Collects the elements into a `Set`.
   *
   *  @tparam B the type of elements in the Set
   */
  def toSet[B >: A] = toBuffer[B].toSet
  /** Collects the elements into a `Vector`. */
  def toVector = toBuffer.toVector
  /** Concatenates the string representations of the elements, surrounded by `start` and `end` and separated by `sep`.
   *
   *  @param start the string to prepend
   *  @param sep the separator between elements
   *  @param end the string to append
   *  @return the concatenated string
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
  /** Concatenates the string representations of the elements, separated by `sep`.
   *
   *  @param sep the separator between elements
   *  @return the concatenated string
   */
  def mkString(sep: String): String = mkString("", sep, "")
  /** Concatenates the string representations of the elements with no separator. */
  def mkString: String = mkString("")


  /** Returns the sum of the elements, using the implicit `Numeric`.
   *
   *  @tparam B the supertype of the elements to sum
   *  @param num the Numeric instance providing zero and addition
   *  @return the sum of the elements
   */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Returns the product of the elements, using the implicit `Numeric`.
   *
   *  @tparam B the supertype of the elements to multiply
   *  @param num the Numeric instance providing one and multiplication
   *  @return the product of the elements
   */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Returns the smallest element, using the implicit `Ordering`.
   *
   *  @tparam B the supertype of the elements to compare
   *  @param cmp the Ordering used to compare elements
   *  @return the smallest element
   */
  def min[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  /** Returns the largest element, using the implicit `Ordering`.
   *
   *  @tparam B the supertype of the elements to compare
   *  @param cmp the Ordering used to compare elements
   *  @return the largest element
   */
  def max[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }

  /** Returns the element for which `f` yields the largest value.
   *
   *  @tparam B the type of the value compared
   *  @param f the function mapping each element to the value to compare
   *  @param cmp the Ordering used to compare the mapped values
   *  @return the element with the largest mapped value
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
  /** Returns the element for which `f` yields the smallest value.
   *
   *  @tparam B the type of the value compared
   *  @param f the function mapping each element to the value to compare
   *  @param cmp the Ordering used to compare the mapped values
   *  @return the element with the smallest mapped value
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
  /** The result of a handler passed to `generate`, controlling whether enumeration continues. */
  sealed trait Action
  object End extends Action
  object Continue extends Action


  /** Creates a Generator over the given elements.
   *
   *  @tparam T the type of the elements
   *  @param xs the elements to generate
   */
  def apply[T](xs: T*) = from(xs)

  /** Creates a Generator over the elements of any collection convertible to `TraversableOnce`.
   *
   *  @tparam M the collection type constructor
   *  @tparam T the type of the elements
   *  @param t the collection to generate elements from
   *  @param convert the implicit conversion from the collection to a `TraversableOnce`
   *  @return a Generator over the elements of `t`
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


  /** Creates a Generator from an iterator and a completion action, which runs after enumeration finishes.
   *
   *  @tparam T the type of the elements
   *  @param makeIterator a by-name expression producing the iterator and the action to run on completion
   *  @return a Generator that runs its completion action after enumeration
   */
  def selfClosing[T](makeIterator: => (Iterator[T], () => Unit)): Generator[T] = new SelfClosing(makeIterator)

  private class SelfClosing[+T](makeIterator: => (Iterator[T], () => Unit)) extends Generator[T]{
    /** Enumerates the elements of the iterator, then runs its completion action.
     *
     *  @param f the handler applied to each element
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
    /** Returns a description of this Generator. */
    override def toString = s"Gen.SelfClosing(...)"
  }



  private class Concat[+T](inner: Generator[T], other: Generator[T]) extends Generator[T] {
    /** Enumerates the elements of `inner`, then those of `other`, unless `inner` ends early.
     *
     *  @param f the handler applied to each element
     */
    def generate(f: T => Generator.Action) = {
      val res1 = inner.generate(f)
      if (res1 == Generator.End) Generator.End
      else other.generate(f)

    }
    /** Returns a description of this Generator. */
    override def toString = s"$inner ++ $other"
  }

  private class ZipWithIndexed[+T](inner: Generator[T]) extends Generator[(T, Int)] {
    /** Enumerates the elements of `inner`, each paired with its index.
     *
     *  @param f the handler applied to each (element, index) pair
     */
    def generate(f: ((T, Int)) => Generator.Action) = {
      var i = 0
      inner.generate{t =>
        val res = f(t, i)
        i += 1
        res
      }
    }
    /** Returns a description of this Generator. */
    override def toString = s"$inner.zipWithIndex"
  }

  private class Zipped[+T, V](inner: Generator[T], other: Iterable[V]) extends Generator[(T, V)] {
    /** Enumerates the elements of `inner` paired with those of `other`, ending when `other` runs out.
     *
     *  @param f the handler applied to each pair
     */
    def generate(f: ((T, V)) => Generator.Action) = {
      val iter = other.iterator
      inner.generate{t =>
        if (!iter.hasNext) Generator.End
        else f(t, iter.next())
      }
    }
    /** Returns a description of this Generator. */
    override def toString = s"$inner.zip($other)"
  }

  private class Filtered[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Enumerates the elements of `inner` that satisfy `pred`.
     *
     *  @param f the handler applied to each kept element
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => if (pred(t)) f(t) else Generator.Continue}
    }
    /** Returns a description of this Generator. */
    override def toString = s"$inner.filter($pred)"
  }

  private class Mapped[+T, V](inner: Generator[V], func: V => T) extends Generator[T]{
    /** Enumerates the elements of `inner` transformed by `func`.
     *
     *  @param f the handler applied to each transformed element
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => f(func(t))}
    }
    /** Returns a description of this Generator. */
    override def toString = s"$inner.map($func)"
  }

  private class FlatMapped[+T, V](inner: Generator[V], func: V => Generator[T]) extends Generator[T]{
    /** Enumerates the elements of each Generator produced by `func` from the elements of `inner`.
     *
     *  @param f the handler applied to each element of the inner Generators
     */
    def generate(f: T => Generator.Action) = {

      inner.generate{ outerT =>
        func(outerT).generate{ innerT =>
          f(innerT)
        }
      }
    }
    /** Returns a description of this Generator. */
    override def toString = s"$inner.map($func)"
  }

  private class Sliding[+T](inner: Generator[T], size: Int, step: Int) extends Generator[Iterable[T]] {
    /** Enumerates the sliding windows of the elements of `inner`.
     *
     *  @param f the handler applied to each window
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
    /** Enumerates the elements of `inner` between indices `start` and `end`.
     *
     *  @param f the handler applied to each element in the slice
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
    /** Returns a description of this Generator. */
    override def toString = s"$inner.slice($start, $end)"
  }

  private class TakeWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Enumerates the elements of `inner` while they satisfy `pred`.
     *
     *  @param f the handler applied to each taken element
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
    /** Returns a description of this Generator. */
    override def toString = s"$inner.takeWhile($pred)"
  }

  private class DropWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Enumerates the elements of `inner` after the leading elements satisfying `pred`.
     *
     *  @param f the handler applied to each remaining element
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
    /** Returns a description of this Generator. */
    override def toString = s"$inner.dropWhile($pred)"
  }

}
