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
 *  @tparam A the type of elements produced by this [[Generator]].
 */
trait Generator[+A]{
  /**
    * The core abstract method that defines the [[Generator]] trait. It is
    * essentially the same as `.foreach`, but with additional configurability.
    *
    * @param handleItem How to handle a single item: performs any desired side
    *                   effects, and returns a [[Generator.Action]] that
    *                   determines how to continue the enumeration.
    * @return the [[Generator.Action]] signalling how enumeration ended:
    *         [[Generator.End]] when downstream requested an early stop,
    *         [[Generator.Continue]] when the upstream was fully consumed.
    */
  def generate(handleItem: A => Generator.Action): Generator.Action


  // Actions
  /** Runs `f` on every element of this [[Generator]] in order, consuming the whole upstream.
   *
   *  @param f the side-effecting function applied to each element in turn; if it throws, enumeration aborts and the exception propagates.
   */
  def foreach(f: A => Unit): Unit = generate{ x =>
    f(x)
    Generator.Continue
  }

  /** Returns the first element satisfying `f`, stopping upstream enumeration as soon as it is found.
   *
   *  @param f the predicate tested against each element in order; if it throws, enumeration aborts and the exception propagates.
   *  @return the first element for which `f` returns true wrapped in `Some`, or `None` when no element satisfies `f`.
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
  /** Returns true when some element satisfies `f`, short-circuiting on the first match.
   *
   *  @param f the predicate tested against each element in order; if it throws, enumeration aborts and the exception propagates.
   */
  def exists(f: A => Boolean) = find(f(_)).isDefined
  /** Returns true when this [[Generator]] contains an element equal to `a`.
   *
   *  @param a the value compared to each element with `==`; any value is accepted, and a value with no equal element simply yields false.
   */
  def contains(a: Any) = exists(_ == a)
  /** Returns true when every element satisfies `f`, short-circuiting on the first failure.
   *
   *  @param f the predicate tested against each element in order; an empty [[Generator]] yields true, and a throwing `f` aborts enumeration with that exception.
   */
  def forall(f: A => Boolean) = !exists(!f(_))
  /** Counts how many elements satisfy `f`, always traversing the whole [[Generator]].
   *
   *  @param f the predicate tested against each element; defaults to always true so a bare `count()` counts every element, and a throwing `f` aborts enumeration with that exception.
   */
  def count(f: A => Boolean = ((_: Any) => true)) = {
    var result = 0
    generate{ t =>
      if (f(t)) result +=  1
      Generator.Continue
    }
    result
  }
  /** Folds all elements left-to-right starting from `start`, delegating to [[foldLeft]].
   *
   *  @tparam B the accumulator and result type, which may differ from `A`.
   *  @param start the initial accumulator value, returned unchanged when this [[Generator]] is empty.
   *  @param f the function combining the current accumulator with the next element; if it throws, enumeration aborts with that exception.
   *  @return the final accumulator after processing every element.
   */
  def fold[B](start: B)(f: (B, A) => B): B = foldLeft(start)(f)
  /** Folds all elements left-to-right starting from `start`, traversing the whole [[Generator]].
   *
   *  @tparam B the accumulator and result type, which may differ from `A`.
   *  @param start the initial accumulator value, returned unchanged when this [[Generator]] is empty.
   *  @param f the function combining the current accumulator with the next element; if it throws, enumeration aborts with that exception.
   *  @return the final accumulator after processing every element.
   */
  def foldLeft[B](start: B)(f: (B, A) => B): B = {
    var result = start
    generate{ t =>
      result = f(result, t)
      Generator.Continue
    }
    result
  }


  /** Reduces all elements with `f` using the first element as the seed, delegating to [[reduceLeft]].
   *
   *  @tparam B the accumulation result type, a supertype of `A` so the first element can seed the accumulation.
   *  @param f the function combining the current accumulation with the next element; if it throws, enumeration aborts with that exception.
   *  @return the combined value; throws `UnsupportedOperationException` when this [[Generator]] is empty.
   */
  def reduce[B >: A](f: (B, A) => B): B = reduceLeft(f)
  /** Reduces all elements with `f` using the first element as the seed, traversing the whole [[Generator]].
   *
   *  @tparam B the accumulation result type, a supertype of `A` so the first element can seed the accumulation.
   *  @param f the function combining the current accumulation with the next element; if it throws, enumeration aborts with that exception.
   *  @return the combined value; throws `UnsupportedOperationException` when this [[Generator]] is empty.
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
  /** Returns a lazy [[Generator]] yielding only the elements for which `pred` returns true.
   *
   *  @param pred the predicate tested against each element; if it throws for an element, enumeration aborts with that exception.
   *  @return a lazy [[Generator]] view; this [[Generator]] is not consumed until the result is enumerated.
   */
  def filter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Returns a lazy [[Generator]] yielding only the elements for which `pred` returns true, for use in for-comprehensions.
   *
   *  @param pred the predicate tested against each element; if it throws for an element, enumeration aborts with that exception.
   *  @return a lazy [[Generator]] view; this [[Generator]] is not consumed until the result is enumerated.
   */
  def withFilter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Returns a lazy [[Generator]] applying `func` to every element of this [[Generator]].
   *
   *  @tparam B the element type of the resulting [[Generator]].
   *  @param func the transformation applied to each element in order; if it throws, enumeration aborts with that exception.
   *  @return a lazy [[Generator]] view; this [[Generator]] is not consumed until the result is enumerated.
   */
  def map[B](func: A => B): Generator[B] = new Generator.Mapped[B, A](this, func)
  /** Returns a lazy [[Generator]] mapping each element to an inner [[Generator]] and enumerating its elements in order.
   *
   *  @tparam B the element type of the inner and resulting [[Generator]]s.
   *  @param func the function producing an inner [[Generator]] for each outer element; if it throws, or an inner [[Generator]] throws, enumeration aborts with that exception.
   *  @return a lazy [[Generator]] view; an upstream [[Generator.End]] from downstream stops both the current inner and the outer enumeration.
   */
  def flatMap[B](func: A => Generator[B]): Generator[B] = new Generator.FlatMapped[B, A](this, func)
  /** Returns a lazy [[Generator]] keeping only elements where `func` is defined, transformed by `func`.
   *
   *  @tparam B the element type of the resulting [[Generator]].
   *  @param func the partial function tested with `isDefinedAt` and then applied; equivalent to `filter(func.isDefinedAt).map(func)`.
   *  @return a lazy [[Generator]] view; this [[Generator]] is not consumed until the result is enumerated.
   */
  def collect[B](func: PartialFunction[A, B]): Generator[B] =
    filter(func.isDefinedAt).map(func)
  /** Returns the first element where `func` is defined, transformed by `func`, stopping enumeration at that match.
   *
   *  @tparam B the result element type produced by `func`.
   *  @param func the partial function tested with `isDefinedAt` and then applied to the first matching element.
   *  @return the transformed first match wrapped in `Some`, or `None` when `func` is defined for no element.
   */
  def collectFirst[B](func: PartialFunction[A, B]): Option[B] =
    filter(func.isDefinedAt).map(func).headOption

  /** Flattens one level by converting each element to a [[Generator]] and concatenating the results via [[flatMap]].
   *
   *  @tparam V the element type of the inner [[Generator]]s and of the resulting [[Generator]].
   *  @param f the implicit conversion turning each element into a [[Generator]]; if it throws for an element, enumeration aborts with that exception.
   */
  def flatten[V](implicit f: A => Generator[V]) = this.flatMap[V]((x: A) => f(x))
  /** Returns a lazy [[Generator]] yielding the elements with indices in `[start, end)`.
   *
   *  @param start the inclusive zero-based index of the first element to yield; a negative value behaves like `0`.
   *  @param end the exclusive zero-based index one past the last element to yield; values beyond the upstream length yield the whole remainder, and `end <= start` yields an empty [[Generator]].
   *  @return a lazy [[Generator]] view; upstream enumeration stops with [[Generator.End]] once `end` elements have been counted.
   */
  def slice(start: Int, end: Int): Generator[A] = new Generator.Sliced(this, start, end)
  /** Groups elements into lazy non-overlapping chunks of `size`, delegating to [[sliding]] with `step == size`.
   *
   *  @param size the number of elements per chunk; must be positive, otherwise the underlying `require` throws `IllegalArgumentException`.
   *  @return a lazy [[Generator]] of `Iterable[A]` chunks in order; the final chunk may hold fewer than `size` elements.
   */
  def grouped(size: Int): Generator[Iterable[A]] = sliding(size, step = size)
  /** Returns lazy sliding windows of `size` elements spaced `step` apart, with a trailing partial window when elements remain.
   *
   *  @param size the window size; must be at least `1`.
   *  @param step the distance between the starts of consecutive windows; must be at least `1`.
   *  @return a lazy [[Generator]] of `Iterable[A]` windows in order; throws `IllegalArgumentException` when `size` or `step` is less than `1`.
   */
  def sliding(size: Int, step: Int): Generator[Iterable[A]] = {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")
    new Generator.Sliding(this, size, step)
  }
  /** Returns a lazy [[Generator]] yielding at most the first `n` elements of this [[Generator]].
   *
   *  @param n the maximum number of elements to yield; `n <= 0` yields an empty [[Generator]], and `n` beyond the upstream length yields the whole [[Generator]].
   */
  def take(n: Int) = slice(0, n)
  /** Returns a lazy [[Generator]] discarding the first `n` elements and yielding the remainder.
   *
   *  @param n the number of leading elements to skip; `n <= 0` yields the whole [[Generator]], and `n` beyond the upstream length yields an empty [[Generator]].
   */
  def drop(n: Int) = slice(n, Int.MaxValue)
  /** Returns a lazy [[Generator]] yielding leading elements while `pred` holds, ending at the first failure.
   *
   *  @param pred the predicate tested against each leading element; the first element with `pred` false ends upstream enumeration with [[Generator.End]].
   *  @return a lazy [[Generator]] view; this [[Generator]] is not consumed beyond the first failing element until the result is enumerated.
   */
  def takeWhile(pred: A => Boolean): Generator[A] = new Generator.TakeWhile(this, pred)
  /** Returns a lazy [[Generator]] skipping leading elements while `pred` holds, then yielding the first failure and everything after it.
   *
   *  @param pred the predicate tested against each leading element; once an element fails `pred`, that element and all later elements are yielded unconditionally.
   *  @return a lazy [[Generator]] view; this [[Generator]] is not consumed until the result is enumerated.
   */
  def dropWhile(pred: A => Boolean): Generator[A] = new Generator.DropWhile(this, pred)
  /** Returns a lazy [[Generator]] pairing each element with its zero-based index. */
  def zipWithIndex: Generator[(A, Int)] = new Generator.ZipWithIndexed(this)
  /** Returns a lazy [[Generator]] pairing each element of this [[Generator]] with the next element of `other` in order.
   *
   *  @tparam B the element type of the `other` collection.
   *  @param other the finite `Iterable` supplying the second half of each pair; enumeration ends with [[Generator.End]] as soon as either side is exhausted.
   *  @return a lazy [[Generator]] of pairs; this [[Generator]] is not consumed until the result is enumerated.
   */
  def zip[B](other: Iterable[B]): Generator[(A, B)] = new Generator.Zipped(this, other)
  /** Returns a lazy [[Generator]] enumerating this [[Generator]] followed by `other`.
   *
   *  @tparam B the common supertype of both [[Generator]]s element types.
   *  @param other the [[Generator]] enumerated after this one; it is skipped when downstream returns [[Generator.End]] during the first half.
   *  @return a lazy concatenated [[Generator]] view; neither input is consumed until the result is enumerated.
   */
  def ++[B >: A](other: Generator[B]): Generator[B] = new Generator.Concat(this, other)

  // Conversions
  /** Returns the first element of this [[Generator]], throwing `NoSuchElementException` when it is empty. */
  def head = take(1).toSeq.head
  /** Returns the first element wrapped in `Some`, or `None` when this [[Generator]] is empty. */
  def headOption = take(1).toSeq.headOption
  /** Collects every element of this [[Generator]] in order into a fresh `mutable.Buffer`.
   *
   *  @tparam B the element type of the buffer, a supertype of `A`.
   *  @return a new buffer holding all elements in encounter order.
   */
  def toBuffer[B >: A]: mutable.Buffer[B] = {
    val arr = mutable.Buffer.empty[B]
    foreach{arr.append(_)}
    arr

  }
  /** Collects every element of this [[Generator]] in order into an `Array` via [[toBuffer]].
   *
   *  @tparam B the array element type, a supertype of `A`, requiring an implicit `ClassTag` to allocate the array.
   *  @return a new array holding all elements in encounter order.
   */
  def toArray[B >: A : ClassTag]: Array[B] = toBuffer.toArray
  /** Collects every element of this [[Generator]] into a `Seq` by delegating to [[toVector]]. */
  def toSeq: Seq[A] = toVector
  /** Collects every element of this [[Generator]] in order into a `List` via [[toBuffer]]. */
  def toList = toBuffer.toList
  /** Collects every element of this [[Generator]] into a `Set`, discarding duplicate elements.
   *
   *  @tparam B the set element type, a supertype of `A`.
   */
  def toSet[B >: A] = toBuffer[B].toSet
  /** Collects every element of this [[Generator]] in order into a `Vector` via [[toBuffer]]. */
  def toVector = toBuffer.toVector
  /** Builds a `String` from all elements with a start, separator, and end, traversing the whole [[Generator]].
   *
   *  @param start the prefix prepended before the first element, even when this [[Generator]] is empty.
   *  @param sep the separator inserted between consecutive elements but never before the first one.
   *  @param end the suffix appended after the last element, even when this [[Generator]] is empty.
   *  @return `start` plus the elements `toString`s joined by `sep` plus `end`.
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
  /** Builds a `String` from all elements joined by `sep` with an empty start and end, delegating to [[mkString]].
   *
   *  @param sep the separator inserted between consecutive elements but never before the first one.
   *  @return the elements `toString`s joined by `sep`.
   */
  def mkString(sep: String): String = mkString("", sep, "")
  /** Concatenates the `toString` of every element with no separator, delegating to [[mkString]]. */
  def mkString: String = mkString("")


  /** Sums all elements with `num`, starting from `num.zero` and traversing the whole [[Generator]].
   *
   *  @tparam B the numeric result type, a supertype of `A`.
   *  @param num the implicit `Numeric` supplying `zero` and `plus`; required to combine elements.
   *  @return the total, or `num.zero` when this [[Generator]] is empty.
   */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Multiplies all elements with `num`, starting from `num.one` and traversing the whole [[Generator]].
   *
   *  @tparam B the numeric result type, a supertype of `A`.
   *  @param num the implicit `Numeric` supplying `one` and `times`; required to combine elements.
   *  @return the product, or `num.one` when this [[Generator]] is empty.
   */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Returns the smallest element according to `cmp`, using [[reduceLeft]] over the whole [[Generator]].
   *
   *  @tparam B the compared supertype of `A` for which the `Ordering` is defined.
   *  @param cmp the implicit `Ordering` used with `lteq` to pick the smaller of two candidates.
   *  @return one of the elements of this [[Generator]]; throws `UnsupportedOperationException` when it is empty.
   */
  def min[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  /** Returns the largest element according to `cmp`, using [[reduceLeft]] over the whole [[Generator]].
   *
   *  @tparam B the compared supertype of `A` for which the `Ordering` is defined.
   *  @param cmp the implicit `Ordering` used with `gteq` to pick the larger of two candidates.
   *  @return one of the elements of this [[Generator]]; throws `UnsupportedOperationException` when it is empty.
   */
  def max[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }

  /** Returns the element whose `f` key is greatest according to `cmp`, keeping the first on ties.
   *
   *  @tparam B the comparison key type produced by `f`.
   *  @param f the function extracting the comparison key from each element; if it throws, enumeration aborts with that exception.
   *  @param cmp the implicit `Ordering` used with `gt` so only a strictly greater key replaces the current best.
   *  @return the winning element; `null` cast to `A` when this [[Generator]] is empty rather than throwing.
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
  /** Returns the element whose `f` key is smallest according to `cmp`, keeping the first on ties.
   *
   *  @tparam B the comparison key type produced by `f`.
   *  @param f the function extracting the comparison key from each element; if it throws, enumeration aborts with that exception.
   *  @param cmp the implicit `Ordering` used with `lt` so only a strictly smaller key replaces the current best.
   *  @return the winning element; `null` cast to `A` when this [[Generator]] is empty rather than throwing.
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
  /** Marker for control-flow signals that a [[generate]] callback returns to steer upstream enumeration. */
  sealed trait Action
  object End extends Action
  object Continue extends Action


  /** Creates a [[Generator]] yielding each of `xs` in order, delegating to [[from]].
   *
   *  @tparam T the type of the supplied elements and of the resulting [[Generator]].
   *  @param xs the varargs elements to yield in order on each enumeration.
   */
  def apply[T](xs: T*) = from(xs)

  /** Wraps any collection convertible to `TraversableOnce` in a [[Generator]] that pulls via `toIterator`.
   *
   *  @tparam M the source container constructor, such as `List` or `Option`.
   *  @tparam T the element type of the source and of the resulting [[Generator]].
   *  @param t the source collection enumerated once per [[generate]] call.
   *  @param convert the implicit conversion exposing `t` as a `TraversableOnce[T]`; a missing or throwing conversion fails at call time.
   *  @return a fresh [[Generator]] that stops pulling early when downstream returns [[Generator.End]].
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


  /** Creates a [[Generator]] that builds a fresh `Iterator` plus cleanup action on every [[generate]] call.
   *
   *  @tparam T the type of elements produced by the iterators.
   *  @param makeIterator the by-name pair of a fresh `Iterator[T]` and its `() => Unit` cleanup; the cleanup always runs in a `finally` block.
   *  @return a [[Generator]] whose [[generate]] runs the cleanup even on early [[Generator.End]] or on exception.
   */
  def selfClosing[T](makeIterator: => (Iterator[T], () => Unit)): Generator[T] = new SelfClosing(makeIterator)

  private class SelfClosing[+T](makeIterator: => (Iterator[T], () => Unit)) extends Generator[T]{
    /** Enumerates the freshly built iterator until exhaustion or [[Generator.End]], always running its cleanup in `finally`.
     *
     *  @param f the callback applied to each iterator element; its [[Generator.End]] stops pulling, and a throw still triggers cleanup before propagating.
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
    /** Returns the fixed placeholder `"Gen.SelfClosing(...)"` without describing the contents. */
    override def toString = s"Gen.SelfClosing(...)"
  }



  private class Concat[+T](inner: Generator[T], other: Generator[T]) extends Generator[T] {
    /** Enumerates `inner` then `other` with `f`, skipping `other` when `inner` stops with [[Generator.End]].
     *
     *  @param f the callback applied to each element of both halves; its [[Generator.End]] is propagated as the result.
     */
    def generate(f: T => Generator.Action) = {
      val res1 = inner.generate(f)
      if (res1 == Generator.End) Generator.End
      else other.generate(f)

    }
    /** Returns `"<inner> ++ <other>"` describing the two concatenated [[Generator]]s. */
    override def toString = s"$inner ++ $other"
  }

  private class ZipWithIndexed[+T](inner: Generator[T]) extends Generator[(T, Int)] {
    /** Pairs each upstream element with its incrementing zero-based index and forwards the pair to `f`.
     *
     *  @param f the callback receiving each `(element, index)` pair; its [[Generator.End]] stops the upstream enumeration.
     */
    def generate(f: ((T, Int)) => Generator.Action) = {
      var i = 0
      inner.generate{t =>
        val res = f(t, i)
        i += 1
        res
      }
    }
    /** Returns `"<inner>.zipWithIndex"` describing the wrapped [[Generator]]. */
    override def toString = s"$inner.zipWithIndex"
  }

  private class Zipped[+T, V](inner: Generator[T], other: Iterable[V]) extends Generator[(T, V)] {
    /** Pairs each upstream element with the next element of `other`, stopping upstream when `other` runs out.
     *
     *  @param f the callback receiving each `(element, otherElement)` pair; returning [[Generator.End]] from `f` or exhausting `other` ends the upstream enumeration.
     */
    def generate(f: ((T, V)) => Generator.Action) = {
      val iter = other.iterator
      inner.generate{t =>
        if (!iter.hasNext) Generator.End
        else f(t, iter.next())
      }
    }
    /** Returns `"<inner>.zip(<other>)"` describing the zipped [[Generator]] and collection. */
    override def toString = s"$inner.zip($other)"
  }

  private class Filtered[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Forwards only upstream elements satisfying `pred`, returning [[Generator.Continue]] for rejected ones.
     *
     *  @param f the callback applied to each accepted element; its [[Generator.End]] stops the upstream enumeration.
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => if (pred(t)) f(t) else Generator.Continue}
    }
    /** Returns `"<inner>.filter(<pred>)"` describing the wrapped [[Generator]] and predicate. */
    override def toString = s"$inner.filter($pred)"
  }

  private class Mapped[+T, V](inner: Generator[V], func: V => T) extends Generator[T]{
    /** Applies `func` to each upstream element and forwards the result to `f`.
     *
     *  @param f the callback applied to each transformed element; its [[Generator.End]] stops the upstream enumeration, and a `func` throw aborts with that exception.
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => f(func(t))}
    }
    /** Returns `"<inner>.map(<func>)"` describing the wrapped [[Generator]] and mapping function. */
    override def toString = s"$inner.map($func)"
  }

  private class FlatMapped[+T, V](inner: Generator[V], func: V => Generator[T]) extends Generator[T]{
    /** Maps each outer element to an inner [[Generator]] and forwards all of its elements to `f` in order.
     *
     *  @param f the callback applied to every inner element; its [[Generator.End]] stops both the current inner and the outer enumeration.
     */
    def generate(f: T => Generator.Action) = {

      inner.generate{ outerT =>
        func(outerT).generate{ innerT =>
          f(innerT)
        }
      }
    }
    /** Returns `"<inner>.map(<func>)"` describing the wrapped [[Generator]] and mapping function. */
    override def toString = s"$inner.map($func)"
  }

  private class Sliding[+T](inner: Generator[T], size: Int, step: Int) extends Generator[Iterable[T]] {
    /** Buffers upstream elements and forwards windows of `size` elements spaced `step` apart, plus a trailing partial window.
     *
     *  @param f the callback receiving each `Iterable[T]` window; its [[Generator.End]] stops upstream enumeration and suppresses the trailing emit.
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
    /** Skips the first `start` elements then forwards elements until index `end`, ending upstream with [[Generator.End]] at the limit.
     *
     *  @param f the callback applied to each in-slice element; evaluating it on the final element and then returning [[Generator.End]] avoids consuming one extra upstream element.
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
    /** Returns `"<inner>.slice(<start>, <end>)"` describing the wrapped [[Generator]] and bounds. */
    override def toString = s"$inner.slice($start, $end)"
  }

  private class TakeWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Forwards leading elements while `pred` holds, returning [[Generator.End]] at the first failure without consuming further upstream.
     *
     *  @param f the callback applied to each accepted leading element; its [[Generator.End]] likewise stops the upstream enumeration.
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
    /** Returns `"<inner>.takeWhile(<pred>)"` describing the wrapped [[Generator]] and predicate. */
    override def toString = s"$inner.takeWhile($pred)"
  }

  private class DropWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Skips leading elements while `pred` holds, then forwards the first failure and every later element unconditionally.
     *
     *  @param f the callback applied to the first failing element and all following ones; its [[Generator.End]] stops the upstream enumeration.
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
    /** Returns `"<inner>.dropWhile(<pred>)"` describing the wrapped [[Generator]] and predicate. */
    override def toString = s"$inner.dropWhile($pred)"
  }

}
