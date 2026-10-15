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
 *  @tparam A the type of the elements this [[Generator]] produces. Every
 *            element is handed to the handler passed to `generate` as an `A`.
 *            The parameter is covariant, so a `Generator[Dog]` can be used
 *            wherever a `Generator[Animal]` is expected; transformations such as
 *            [[map]] may change `A` entirely, but may never narrow it.
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
  /** Runs `f` on every element of this [[Generator]], in enumeration order. The
   *  control-flow information of [[Generator.generate]] is discarded: every
   *  element is handed on and [[Generator.Continue]] is always returned
   *  upstream, so the upstream [[Generator]] is enumerated to exhaustion.
   *
   *  @param f the side effect to perform for each element. It is called exactly
   *           once per element; an exception thrown by `f` propagates out of
   *           `foreach` and abandons the upstream enumeration.
   */
  def foreach(f: A => Unit): Unit = generate{ x =>
    f(x)
    Generator.Continue
  }

  /** Enumerates this [[Generator]] until it finds the first element for which
   *  `f` is `true`, then stops the upstream enumeration immediately with
   *  [[Generator.End]]. No element after the match is ever generated.
   *
   *  @param f the test applied to each element, one element at a time. An
   *           exception it throws propagates out of `find` and abandons the
   *           upstream enumeration.
   *  @return `Some` of the first matching element, or `None` if no element
   *          matched - which is also what an empty [[Generator]] gives back.
   *          The two cases cannot be told apart from the result. The call only
   *          returns once enumeration terminates, so it never returns on an
   *          endless [[Generator]] that keeps failing the test.
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
  /** Returns whether any element of this [[Generator]] satisfies `f`, stopping
   *  the enumeration as soon as one does. Implemented as `find(f(_)).isDefined`,
   *  so an empty [[Generator]] is `false`.
   *
   *  @param f the test applied to each element until it matches; elements after
   *           the match are not generated.
   */
  def exists(f: A => Boolean) = find(f(_)).isDefined
  /** Returns whether any element of this [[Generator]] is equal to `a`, stopping
   *  the enumeration at the first such element. Implemented as `exists(_ == a)`.
   *
   *  @param a the value compared with `==` against every element. It is typed as
   *           `Any` so that a value of any type can be passed, including
   *           `null`; whether anything matches is entirely up to the elements'
   *           `equals` implementations, so this is `false` for an empty
   *           [[Generator]] and for incomparable types.
   */
  def contains(a: Any) = exists(_ == a)
  /** Returns whether every element of this [[Generator]] satisfies `f`.
   *  Implemented as `!exists(!f(_))`, so enumeration stops at the first element
   *  that fails `f`, and an empty [[Generator]] is vacuously `true`.
   *
   *  @param f the test applied to each element. Elements after the first
   *           failure are never generated, so `f` is not called on them.
   */
  def forall(f: A => Boolean) = !exists(!f(_))
  /** Counts the elements of this [[Generator]] that satisfy `f`. Enumeration
   *  always continues, so the whole upstream [[Generator]] is forced: this
   *  never stops early, however few elements pass `f`.
   *
   *  @param f the test deciding whether an element is counted. It defaults to a
   *           predicate that is `true` for every element, so a call with no
   *           argument returns the total number of elements.
   */
  def count(f: A => Boolean = ((_: Any) => true)) = {
    var result = 0
    generate{ t =>
      if (f(t)) result +=  1
      Generator.Continue
    }
    result
  }
  /** Folds the elements of this [[Generator]] into a single value, from left to
   *  right. This is a synonym for [[foldLeft]]; see there for the details.
   *
   *  @tparam B the type of the accumulator. It is unconstrained and unrelated
   *            to `A`.
   *  @param start the accumulator to start from. It is returned unchanged if
   *               this [[Generator]] produces no elements.
   *  @param f called with the current accumulator and the next element, in that
   *           order; its result becomes the new accumulator.
   *  @return the accumulator after the last element, or `start` if this
   *          [[Generator]] is empty.
   */
  def fold[B](start: B)(f: (B, A) => B): B = foldLeft(start)(f)
  /** Evaluates this [[Generator]] and folds its elements into a single value
   *  from left to right.
   *
   *  @tparam B the type of the accumulator. It is unconstrained and unrelated
   *            to `A`.
   *  @param start the accumulator to start from. It is returned unchanged if
   *               this [[Generator]] produces no elements.
   *  @param f called with the current accumulator and the next element, in that
   *           order; its result becomes the new accumulator. An exception it
   *           throws propagates out of `foldLeft` and abandons the rest of the
   *           enumeration.
   *  @return the accumulator after the last element, or `start` if this
   *          [[Generator]] is empty.
   */
  def foldLeft[B](start: B)(f: (B, A) => B): B = {
    var result = start
    generate{ t =>
      result = f(result, t)
      Generator.Continue
    }
    result
  }


  /** Combines the elements of this [[Generator]] into a single value of a
   *  supertype of `A`, with no initial value. This is a synonym for
   *  [[reduceLeft]]; see there for the details.
   *
   *  @tparam B the type of the result. It must be a supertype of `A`, because
   *            the first generated element is used as the initial accumulator.
   *  @param f called with the current accumulator and the next element, in that
   *           order.
   *  @return the combined value, or a thrown
   *          `UnsupportedOperationException("empty.reduceLeft")` if this
   *          [[Generator]] is empty.
   */
  def reduce[B >: A](f: (B, A) => B): B = reduceLeft(f)
  /** Evaluates this [[Generator]] and combines all of its elements from left
   *  to right, using the first generated element as the initial accumulator.
   *
   *  @tparam B the type of the result. It must be a supertype of `A`, since the
   *            first element seeds the accumulator without being combined with
   *            anything.
   *  @param f called with the current accumulator and the next element, in that
   *           order. For a result that does not depend on the enumeration
   *           order it should be associative, and typically commutative.
   *  @return the accumulator after the last element. Throws
   *          `UnsupportedOperationException("empty.reduceLeft")` if this
   *          [[Generator]] is empty, since there is no element to seed the
   *          accumulator with.
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
  /** Returns a lazy [[Generator]] that yields only the elements for which
   *  `pred` is `true`. Nothing is evaluated here: the predicate is only run
   *  when the returned [[Generator]] is itself enumerated, and it is run again
   *  from scratch on every enumeration.
   *
   *  @param pred the test applied to each element of the upstream
   *              [[Generator]]. Elements that fail it are skipped entirely and
   *              never reach the downstream handler.
   *  @return a new [[Generator]] over the surviving elements, in their original
   *          order.
   */
  def filter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Behaves exactly like [[filter]]: returns a lazy [[Generator]] over the
   *  elements that satisfy `pred`. The synonym exists so that `for`/`yield`
   *  comprehensions, which desugar to `withFilter(...).map(...)`, go through
   *  [[Generator]]'s own filtering rather than `scala.collection`'s.
   *
   *  @param pred the test applied to each element of the upstream
   *              [[Generator]]. Elements that fail it are skipped entirely and
   *              never reach the downstream handler.
   *  @return a new [[Generator]] over the surviving elements, in their original
   *          order.
   */
  def withFilter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Returns a lazy [[Generator]] that applies `func` to every element of this
   *  one. Elements are pulled from upstream one at a time and transformed
   *  immediately; nothing is buffered or cached, so re-enumerating the result
   *  re-applies `func`.
   *
   *  @tparam B the type `func` produces. It is unconstrained, so `map` may
   *            change the element type, including widening or discarding it.
   *  @param func applied to each element in enumeration order. An exception it
   *              throws propagates out and abandons the upstream enumeration.
   *  @return a new [[Generator[B]] that hands `func`'s results to the
   *          downstream handler.
   */
  def map[B](func: A => B): Generator[B] = new Generator.Mapped[B, A](this, func)
  /** Returns a lazy [[Generator]] that, for each element of this one, enumerates
   *  the [[Generator]] that `func` returns and splices its elements into the
   *  result. The inner generators are enumerated in order, one at a time, as the
   *  outer elements are generated.
   *
   *  @tparam B the element type of the [[Generator]]s that `func` returns.
   *  @param func applied to each element of the upstream [[Generator]]; it must
   *              return a non-`null` [[Generator]], since a `null` result makes
   *              the subsequent enumeration fail with a
   *              `NullPointerException`.
   *  @return a new [[Generator[B]] over the concatenation of all the inner
   *          generators. Returning [[Generator.End]] from the downstream
   *          handler stops both the current inner generator and the outer
   *          enumeration, because that action is what the upstream
   *          [[Generator]] sees.
   */
  def flatMap[B](func: A => Generator[B]): Generator[B] = new Generator.FlatMapped[B, A](this, func)
  /** Returns a lazy [[Generator]] that maps the elements for which `func` is
   *  defined and drops the rest. Defined as
   *  `filter(func.isDefinedAt).map(func)`.
   *
   *  @tparam B the type produced by the defined cases of `func`.
   *  @param func the partial function to apply. `isDefinedAt` decides which
   *              elements survive; an element for which `isDefinedAt` is `true`
   *              but whose body is not defined throws `MatchError` when the
   *              result is enumerated.
   *  @return a new [[Generator[B]] over the mapped elements, in their original
   *          order.
   */
  def collect[B](func: PartialFunction[A, B]): Generator[B] =
    filter(func.isDefinedAt).map(func)
  /** Enumerates this [[Generator]] until the first element for which `func` is
   *  defined, applies `func` to that one element, and stops. Unlike [[collect]]
   *  this is an immediate action rather than a lazy [[Generator]]: the upstream
   *  elements are generated during the call.
   *
   *  @tparam B the type produced by the defined cases of `func`.
   *  @param func the partial function used both to select and to transform the
   *              element. An exception from a defined body propagates to the
   *              caller instead of being turned into `None`.
   *  @return `Some` of `func`'s result for the first element it is defined for,
   *          or `None` if this [[Generator]] was exhausted without a match.
   */
  def collectFirst[B](func: PartialFunction[A, B]): Option[B] =
    filter(func.isDefinedAt).map(func).headOption

  /** Returns a lazy [[Generator]] that treats every element of this one as a
   *  [[Generator]] itself and yields all of their elements, concatenated. This
   *  is `flatMap` with that conversion applied.
   *
   *  @tparam V the element type of the inner [[Generator]]s that each element
   *            of this one is converted into.
   *  @param f the implicit conversion from an element to the [[Generator]] it
   *           stands for. A call without a matching conversion in scope does
   *           not compile; one that yields `null` fails at enumeration time
   *           with a `NullPointerException`.
   */
  def flatten[V](implicit f: A => Generator[V]) = this.flatMap[V]((x: A) => f(x))
  /** Returns a lazy [[Generator]] over the elements whose zero-based index lies
   *  in the half-open range `start` until `end`. Elements before `start` are
   *  still generated from upstream and then discarded, so slicing is not free
   *  of upstream cost.
   *
   *  @param start the index of the first element to keep. A negative value
   *               behaves like `0`, and one past the end of the upstream
   *               [[Generator]] leaves nothing to keep.
   *  @param end the index one past the last element to keep. Enumeration is
   *             stopped as soon as that index is reached, so no element beyond
   *             the range is generated.
   *  @return a new [[Generator]] over the elements in range. Nothing is
   *          validated, so a range with `start >= end`, or with a negative
   *          `end`, simply produces no elements instead of throwing.
   */
  def slice(start: Int, end: Int): Generator[A] = new Generator.Sliced(this, start, end)
  /** Returns a lazy [[Generator]] of consecutive, non-overlapping chunks of
   *  `size` elements, delegating to `sliding(size, step = size)`. The last chunk
   *  is short when the number of elements is not a multiple of `size`.
   *
   *  @param size the maximum number of elements per chunk.
   *  @return a new [[Generator]] of `Iterable` chunks, emitted as the upstream
   *          elements are consumed.
   *  @throws IllegalArgumentException if `size` is below `1`, as required by
   *          [[sliding]].
   */
  def grouped(size: Int): Generator[Iterable[A]] = sliding(size, step = size)
  /** Returns a lazy [[Generator]] of overlapping windows of at most `size`
   *  elements, starting a new window every `step` elements. Each window is
   *  built up and materialised at the moment it is emitted, so with
   *  `step < size` consecutive windows overlap.
   *
   *  @param size the maximum number of elements per window; must be at least
   *              `1`.
   *  @param step how far the window advances between emissions; must be at least
   *              `1`.
   *  @return a new [[Generator]] of windows, the last of which may be shorter
   *          than `size`. It is only emitted after the upstream [[Generator]] is
   *          exhausted and only if the handler's last action was
   *          [[Generator.Continue]].
   *  @throws IllegalArgumentException if either argument is below `1`, with the
   *          message `size=%d and step=%d, but both must be positive`.
   */
  def sliding(size: Int, step: Int): Generator[Iterable[A]] = {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")
    new Generator.Sliding(this, size, step)
  }
  /** Returns a lazy [[Generator]] over at most the first `n` elements of this
   *  one. Defined as `slice(0, n)`, so enumeration of the upstream
   *  [[Generator]] stops as soon as the limit is reached.
   *
   *  @param n the maximum number of elements to keep. `0` or less yields an
   *           empty [[Generator]] rather than throwing, and an `n` larger than
   *           the number of available elements keeps all of them.
   */
  def take(n: Int) = slice(0, n)
  /** Returns a lazy [[Generator]] with the first `n` elements removed. Defined
   *  as `slice(n, Int.MaxValue)`; the skipped elements are still generated from
   *  upstream and then discarded.
   *
   *  @param n the number of leading elements to skip. `0` or less drops
   *           nothing, and an `n` at or beyond the number of available elements
   *           leaves an empty [[Generator]].
   */
  def drop(n: Int) = slice(n, Int.MaxValue)
  /** Returns a lazy [[Generator]] over the leading elements for which `pred` is
   *  `true`, ending as soon as it is `false`. The first failing element is still
   *  generated from upstream in order to make that decision, but it is not
   *  part of the result.
   *
   *  @param pred the test applied to each element in turn. Once it returns
   *              `false` no further element is generated, and `pred` is not
   *              called again.
   *  @return a new [[Generator]] over the leading matching elements, in order.
   */
  def takeWhile(pred: A => Boolean): Generator[A] = new Generator.TakeWhile(this, pred)
  /** Returns a lazy [[Generator]] that discards the leading elements for which
   *  `pred` is `true` and keeps everything from the first element onwards. The
   *  first element that fails `pred` is the first one included in the result.
   *
   *  @param pred the test applied only while nothing has been emitted yet. It is
   *              not consulted for any later element, so those are kept no
   *              matter what the predicate says.
   *  @return a new [[Generator]] over the remaining elements, in order.
   */
  def dropWhile(pred: A => Boolean): Generator[A] = new Generator.DropWhile(this, pred)
  /** Returns a lazy [[Generator]] that pairs every element of this one with
   *  its zero-based index. The counter counts the elements this [[Generator]]
   *  actually produces, so it starts at `0` here even when the upstream
   *  generator had already dropped elements.
   */
  def zipWithIndex: Generator[(A, Int)] = new Generator.ZipWithIndexed(this)
  /** Returns a lazy [[Generator]] of pairs, pairing each element of this
   *  [[Generator]] with the corresponding element of `other`. It stops as soon
   *  as either side runs out, so the result is as long as the shorter of the
   *  two.
   *
   *  @tparam B the element type of `other`.
   *  @param other the iterable to zip against. A fresh `iterator` is requested
   *              from it for each enumeration, so re-enumerating the result
   *              starts from the beginning of `other` again.
   *  @return a new [[Generator]] of pairs.
   */
  def zip[B](other: Iterable[B]): Generator[(A, B)] = new Generator.Zipped(this, other)
  /** Returns a lazy [[Generator]] that enumerates this one and then `other`.
   *  `other` is only enumerated if this [[Generator]] runs to completion: if
   *  the downstream handler returns [[Generator.End]] while this
   *  [[Generator]] is still producing elements, `other` is never touched.
   *
   *  @tparam B the common element type of the two generators.
   *  @param other the [[Generator]] whose elements follow this one's.
   *  @return a new [[Generator]] over the concatenation of the two, in that
   *          order.
   */
  def ++[B >: A](other: Generator[B]): Generator[B] = new Generator.Concat(this, other)

  // Conversions
  /** Evaluates enough of this [[Generator]] to obtain its first element,
   *  stopping there; the remaining elements are never generated.
   *
   *  @return the first element of this [[Generator]].
   *  @throws NoSuchElementException if this [[Generator]] is empty. Use
   *          [[headOption]] where an empty [[Generator]] is a valid input.
   */
  def head = take(1).toSeq.head
  /** Evaluates enough of this [[Generator]] to obtain its first element,
   *  stopping there; the remaining elements are never generated.
   *
   *  @return `Some` of the first element, or `None` if this [[Generator]] is
   *          empty, which makes this the safe counterpart to [[head]].
   */
  def headOption = take(1).toSeq.headOption
  /** Evaluates this [[Generator]] and collects all of its elements into a
   *  mutable buffer. This forces the upstream enumeration in full, ignoring any
   *  opportunity to stop early.
   *
   *  @tparam B the element type of the buffer. It must be a supertype of `A`,
   *            which is what lets callers widen the elements while converting.
   *  @return a fresh `mutable.Buffer[B]` holding the elements in enumeration
   *          order. The buffer is mutable and owned by the caller.
   */
  def toBuffer[B >: A]: mutable.Buffer[B] = {
    val arr = mutable.Buffer.empty[B]
    foreach{arr.append(_)}
    arr

  }
  /** Evaluates this [[Generator]] and copies all of its elements into a new
   *  array. This forces the upstream enumeration in full. The
   *  [[scala.reflect.ClassTag]] context bound is what allows the array to be
   *  created with the correct runtime component type, so a call for which no
   *  instance is available does not compile.
   *
   *  @tparam B the element type of the array. It must be a supertype of `A`.
   *  @return a new `Array[B]` with one slot per generated element, in
   *          enumeration order.
   */
  def toArray[B >: A : ClassTag]: Array[B] = toBuffer.toArray
  /** Evaluates this [[Generator]] and returns all of its elements as an
   *  immutable `Seq`, delegating to [[toVector]].
   */
  def toSeq: Seq[A] = toVector
  /** Evaluates this [[Generator]] and returns all of its elements as an
   *  immutable `List`, in enumeration order.
   */
  def toList = toBuffer.toList
  /** Evaluates this [[Generator]] and returns its elements as a `Set`, so
   *  duplicates and the original ordering are both discarded.
   *
   *  @tparam B the element type of the set. It must be a supertype of `A`.
   */
  def toSet[B >: A] = toBuffer[B].toSet
  /** Evaluates this [[Generator]] and returns all of its elements as a
   *  `Vector`, in enumeration order.
   */
  def toVector = toBuffer.toVector
  /** Evaluates this [[Generator]] and renders each element into a single
   *  string via `String.valueOf`, wrapping the whole thing in `start` and `end`
   *  and joining the elements with `sep`. None of the delimiters are escaped, so
   *  an element whose rendering contains `sep` is indistinguishable from a real
   *  separator.
   *
   *  @param start prepended to the result, e.g. `"("`.
   *  @param sep inserted between consecutive elements only: never after `start`
   *              and never before `end`.
   *  @param end appended to the result, e.g. `")"`.
   *  @return `start`, the elements joined by `sep`, and `end`. An empty
   *          [[Generator]] therefore yields `start + end`, with no separator.
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
  /** Evaluates this [[Generator]] and joins the rendered elements with `sep`,
   *  with no leading or trailing delimiter. Defined as `mkString("", sep, "")`.
   *
   *  @param sep inserted between consecutive elements.
   *  @return the joined elements, or `""` if this [[Generator]] is empty: the
   *          separator is not emitted on its own.
   */
  def mkString(sep: String): String = mkString("", sep, "")
  /** Evaluates this [[Generator]] and concatenates the rendered elements with
   *  no delimiter at all, which is `mkString("")`.
   *
   *  @return the concatenation of all the elements, or `""` if this
   *          [[Generator]] is empty.
   */
  def mkString: String = mkString("")


  /** Evaluates this [[Generator]] and adds up its elements, from left to right.
   *
   *  @tparam B the numeric type of the result. It must be a supertype of `A`.
   *  @param num the `Numeric` instance supplying `zero` and `plus`. A call with
   *             no matching instance in scope does not compile; an instance that
   *             does not support `B` fails at runtime.
   *  @return the sum, or `num.zero` if this [[Generator]] is empty, since `0`
   *          is the initial accumulator.
   */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Evaluates this [[Generator]] and multiplies its elements, from left to
   *  right.
   *
   *  @tparam B the numeric type of the result. It must be a supertype of `A`.
   *  @param num the `Numeric` instance supplying `one` and `times`. A call with
   *             no matching instance in scope does not compile; an instance
   *             that does not support `B` fails at runtime.
   *  @return the product, or `num.one` if this [[Generator]] is empty, since `1`
   *          is the initial accumulator.
   */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Evaluates this [[Generator]] and returns its smallest element according
   *  to `cmp`. Implemented as a [[reduceLeft]] that keeps `x` whenever `cmp`
   *  says `x` is less than or equal to `y`, so among equal elements the
   *  earliest one is returned.
   *
   *  @tparam B the type `cmp` orders. It must be a supertype of `A`, so that
   *            the first element can seed the comparison.
   *  @param cmp the `Ordering` used to compare elements. It should be a total
   *             ordering for the result to be meaningful; a call with no
   *             matching instance in scope does not compile.
   *  @return the smallest element, typed as `A`.
   *  @throws UnsupportedOperationException if this [[Generator]] is empty,
   *          which comes from the underlying [[reduceLeft]].
   */
  def min[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  /** Evaluates this [[Generator]] and returns its largest element according to
   *  `cmp`. Implemented as a [[reduceLeft]] that keeps `x` whenever `cmp` says
   *  `x` is greater than or equal to `y`, so among equal elements the earliest
   *  one is returned.
   *
   *  @tparam B the type `cmp` orders. It must be a supertype of `A`, so that
   *            the first element can seed the comparison.
   *  @param cmp the `Ordering` used to compare elements. It should be a total
   *             ordering for the result to be meaningful; a call with no
   *             matching instance in scope does not compile.
   *  @return the largest element, typed as `A`.
   *  @throws UnsupportedOperationException if this [[Generator]] is empty,
   *          which comes from the underlying [[reduceLeft]].
   */
  def max[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }

  /** Evaluates this [[Generator]] and returns the element whose `f` value is
   *  largest. Only a strictly greater key replaces the current maximum, so of
   *  several elements with equal maximum key the first one is returned.
   *
   *  @tparam B the type of the key `f` computes. It is unrelated to `A` and is
   *            used only for the comparison.
   *  @param f computes the key of an element; it is called once per generated
   *           element, and must return a non-`null` key unless this
   *           [[Generator]] holds a single element, since a `null` key is
   *           passed to `cmp` and makes the comparison throw.
   *  @param cmp the `Ordering` of the keys. A call with no matching instance in
   *             scope does not compile.
   *  @return the element with the largest key. Unlike [[max]], an empty
   *          [[Generator]] does not throw here: it yields `null`, so the result
   *          has to be checked before it is used.
   *  @throws java.lang.NullPointerException from the comparison if `f` returns
   *          `null` for any element after the first.
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
  /** Evaluates this [[Generator]] and returns the element whose `f` value is
   *  smallest. Only a strictly smaller key replaces the current minimum, so of
   *  several elements with equal minimum key the first one is returned.
   *
   *  @tparam B the type of the key `f` computes. It is unrelated to `A` and is
   *            used only for the comparison.
   *  @param f computes the key of an element; it is called once per generated
   *           element, and must return a non-`null` key unless this
   *           [[Generator]] holds a single element, since a `null` key is
   *           passed to `cmp` and makes the comparison throw.
   *  @param cmp the `Ordering` of the keys. A call with no matching instance in
   *             scope does not compile.
   *  @return the element with the smallest key. Unlike [[min]], an empty
   *          [[Generator]] does not throw here: it yields `null`, so the result
   *          has to be checked before it is used.
   *  @throws java.lang.NullPointerException from the comparison if `f` returns
   *          `null` for any element after the first.
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
  /** The control-flow token that the handler passed to
   *  [[Generator.generate]] returns for each element: it tells the upstream
   *  [[Generator]] whether to keep producing elements. There are exactly two
   *  cases: [[Generator.Continue]], meaning "keep enumerating", and
   *  [[Generator.End]], meaning "stop right now without generating any further
   *  elements". Being sealed, no other action can exist.
   */
  sealed trait Action
  object End extends Action
  object Continue extends Action


  /** Builds a [[Generator]] over the given values, so that a list of arguments
   *  can be used wherever a [[Generator]] is expected. Delegates to
   *  [[Generator.from]], so the result can be enumerated more than once.
   *
   *  @tparam T the common element type of `xs`.
   *  @param xs the elements the result yields, in the order given. It is a
   *            repeated parameter, so passing none produces an empty
   *            [[Generator]] rather than an error.
   */
  def apply[T](xs: T*) = from(xs)

  /** The implicit conversion that lifts a container of elements into a
   *  [[Generator]], which is what allows `Gen`-based code to accept a `List`,
   *  `Seq` or similar directly.
   *
   *  @tparam M the container type constructor of `t`, e.g. `List` or `Vector`.
   *  @tparam T the element type of that container.
   *  @param t the container to expose as a [[Generator]]. Its elements are
   *           pulled from an iterator created afresh on each enumeration, so the
   *           result can be walked more than once; the container itself is
   *           neither consumed nor modified.
   *  @param convert the implicit function that turns `t` into a
   *                `TraversableOnce`. A call with no matching instance in
   *                scope does not compile.
   *  @return a [[Generator[T]] whose `generate` pulls elements from the
   *          converted container until the iterator is exhausted or the handler
   *          returns something other than [[Generator.Continue]]. It returns the
   *          handler's last action, or [[Generator.Continue]] if the container is
   *          empty and the handler was never called.
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


  /** Builds a [[Generator]] that guarantees a cleanup step once its enumeration
   *  is over, for sources that hold a resource such as a file handle. The
   *  cleanup is tied to the enumeration rather than to the construction, so it
   *  still runs when the consumer stops early with [[Generator.End]].
   *
   *  @tparam T the element type produced by the iterator.
   *  @param makeIterator a by-name block producing the iterator to enumerate
   *                      together with the cleanup function to run afterwards.
   *                      It is evaluated afresh on every enumeration, so the
   *                      [[Generator]] can be walked more than once, and the
   *                      iterator it returns must itself be lazy, since the
   *                      cleanup may be called while it is still unexhausted.
   *  @return a [[Generator]] that runs the cleanup after each enumeration,
   *          whether the enumeration completed, was cut short with
   *          [[Generator.End]], or threw.
   */
  def selfClosing[T](makeIterator: => (Iterator[T], () => Unit)): Generator[T] = new SelfClosing(makeIterator)

  private class SelfClosing[+T](makeIterator: => (Iterator[T], () => Unit)) extends Generator[T]{
    /** Enumerates the iterator built by `makeIterator` and runs the cleanup
     *  function afterwards in a `finally`, so the cleanup happens even if the
     *  handler throws.
     *
     *  @param f the handler for each element. Returning [[Generator.End]]
     *           stops the enumeration early; the cleanup still runs.
     *  @return the last action the handler returned, or [[Generator.Continue]]
     *          if the iterator was empty and the handler was never called.
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
    /** Returns the fixed string `"Gen.SelfClosing(...)"`. The underlying
     *  iterator is deliberately not shown, since `makeIterator` may not have
     *  been evaluated yet.
     */
    override def toString = s"Gen.SelfClosing(...)"
  }



  private class Concat[+T](inner: Generator[T], other: Generator[T]) extends Generator[T] {
    /** Enumerates `inner`, then enumerates `other` if `inner` ran to
     *  completion.
     *
     *  @param f the handler, passed straight through to whichever generator is
     *           currently being enumerated.
     *  @return [[Generator.End]] if `inner` was ended by the handler, in which
     *          case `other` is never enumerated. Otherwise the result of
     *          enumerating `other`: the handler's last action, or
     *          [[Generator.Continue]] if `other` is empty.
     */
    def generate(f: T => Generator.Action) = {
      val res1 = inner.generate(f)
      if (res1 == Generator.End) Generator.End
      else other.generate(f)

    }
    /** Returns `"<inner> ++ <other>"`, with both operands rendered by their own
     *  `toString`.
     */
    override def toString = s"$inner ++ $other"
  }

  private class ZipWithIndexed[+T](inner: Generator[T]) extends Generator[(T, Int)] {
    /** Enumerates `inner`, numbering its elements from zero and passing each
     *  `(element, index)` pair to the handler.
     *
     *  @param f the handler for each pair. The counter is advanced after `f`
     *           returns, including on the final call where `f` returned
     *           [[Generator.End]].
     *  @return the last action from `f`, forwarded from `inner` unchanged.
     */
    def generate(f: ((T, Int)) => Generator.Action) = {
      var i = 0
      inner.generate{t =>
        val res = f(t, i)
        i += 1
        res
      }
    }
    /** Returns `"<inner>.zipWithIndex"`, using [[zipWithIndex]]'s own textual
     *  form.
     */
    override def toString = s"$inner.zipWithIndex"
  }

  private class Zipped[+T, V](inner: Generator[T], other: Iterable[V]) extends Generator[(T, V)] {
    /** Enumerates `inner` alongside `other`, pairing each element of `inner`
     *  with the next element of `other`.
     *
     *  @param f the handler for each pair. When `other` is already exhausted the
     *           handler is not called at all; [[Generator.End]] is returned
     *           instead, which stops `inner` without generating any further
     *           element from it.
     *  @return the last action from `f`, or [[Generator.End]] if `other` ran
     *          out first. This is `inner`'s own result, so an empty `inner`
     *          yields [[Generator.Continue]].
     */
    def generate(f: ((T, V)) => Generator.Action) = {
      val iter = other.iterator
      inner.generate{t =>
        if (!iter.hasNext) Generator.End
        else f(t, iter.next())
      }
    }
    /** Returns `"<inner>.zip(<other>)"`, rendering the zipped iterable by its
     *  own `toString`.
     */
    override def toString = s"$inner.zip($other)"
  }

  private class Filtered[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Enumerates `inner` and forwards only the elements that satisfy `pred` to
     *  the handler.
     *
     *  @param f the handler for the surviving elements. Its action is passed
     *           straight back to `inner`, so [[Generator.End]] from `f` stops
     *           the upstream enumeration too.
     *  @return whatever action `inner`'s enumeration ended with.
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => if (pred(t)) f(t) else Generator.Continue}
    }
    /** Returns `"<inner>.filter(<pred>)"`, printing the predicate function
     *  itself rather than any result of it.
     */
    override def toString = s"$inner.filter($pred)"
  }

  private class Mapped[+T, V](inner: Generator[V], func: V => T) extends Generator[T]{
    /** Enumerates `inner`, applies `func` to each element, and passes the
     *  results to the handler.
     *
     *  @param f the handler for the mapped elements. An exception thrown by
     *           `func` or by `f` propagates out and abandons the enumeration of
     *           `inner`.
     *  @return the last action from `f`, forwarded from `inner` unchanged.
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => f(func(t))}
    }
    /** Returns `"<inner>.map(<func>)"`, printing the mapping function itself
     *  rather than any result of it.
     */
    override def toString = s"$inner.map($func)"
  }

  private class FlatMapped[+T, V](inner: Generator[V], func: V => Generator[T]) extends Generator[T]{
    /** Enumerates `inner` and, for each of its elements, enumerates the
     *  [[Generator]] that `func` returns, forwarding that generator's elements
     *  to the handler.
     *
     *  @param f the handler for the elements of the inner generators. Its action
     *           is what the inner [[Generator]] sees, so returning
     *           [[Generator.End]] stops that inner generator and also stops the
     *           outer enumeration, since that same action reaches `inner`.
     *  @return the action `inner` ends with, which is the action returned by
     *          the last inner generator enumerated, or [[Generator.Continue]] if
     *          `inner` produced no elements.
     */
    def generate(f: T => Generator.Action) = {

      inner.generate{ outerT =>
        func(outerT).generate{ innerT =>
          f(innerT)
        }
      }
    }
    /** Returns `"<inner>.map(<func>)"`. The name says `map` even though this is
     *  the class built by [[flatMap]].
     */
    override def toString = s"$inner.map($func)"
  }

  private class Sliding[+T](inner: Generator[T], size: Int, step: Int) extends Generator[Iterable[T]] {
    /** Enumerates `inner`, buffering up to `max(size, step)` elements and
     *  handing the handler a window of at most `size` elements every `step`
     *  elements. Any trailing elements are emitted as one short final window,
     *  but only once `inner` is exhausted and only if the handler's last action
     *  was [[Generator.Continue]].
     *
     *  @param f the handler for each window. If it returns [[Generator.End]] the
     *           window that was handed to it has still been seen by it, but
     *           whatever is still buffered is discarded and never emitted.
     *  @return the last action from `f`, or [[Generator.Continue]] if `inner`
     *          was empty and the handler was therefore never called.
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
    /** Enumerates `inner`, discarding the first `start` elements and passing on
     *  the elements from index `start` up to, but not including, `end`.
     *
     *  @param f the handler for the elements in range. The element at index
     *           `end` is never generated: once the last in-range element has
     *           been passed to `f`, [[Generator.End]] is sent upstream.
     *  @return the action `inner` ended with: [[Generator.End]] if the end of
     *          the range was reached first, or the result of enumerating `inner`
     *          if that ran out before the range did.
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
    /** Returns `"<inner>.slice(<start>, <end>)"`, showing the bounds as they
     *  were handed to [[slice]].
     */
    override def toString = s"$inner.slice($start, $end)"
  }

  private class TakeWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Enumerates `inner` for as long as `pred` holds, passing each accepted
     *  element to the handler.
     *
     *  @param f the handler for the accepted elements. The first element that
     *           fails `pred` is consumed from `inner` in order to make that
     *           decision but is not passed to `f`; [[Generator.End]] is then
     *           sent upstream.
     *  @return [[Generator.End]] if `pred` failed before `inner` was exhausted,
     *          otherwise the result of enumerating `inner`.
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
    /** Returns `"<inner>.takeWhile(<pred>)"`, printing the predicate function
     *  itself rather than any result of it.
     */
    override def toString = s"$inner.takeWhile($pred)"
  }

  private class DropWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Enumerates `inner`, skipping the leading elements for which `pred` is
     *  `true` and passing the first failing element, and every element after
     *  it, to the handler.
     *
     *  @param f the handler for the retained elements. It is not called at all if
     *           `pred` held for every element of `inner`.
     *  @return the action `inner` ended with, forwarded unchanged; nothing
     *          shortens the enumeration when the skipped prefix runs to the end.
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
    /** Returns `"<inner>.dropWhile(<pred>)"`, printing the predicate function
     *  itself rather than any result of it.
     */
    override def toString = s"$inner.dropWhile($pred)"
  }

}