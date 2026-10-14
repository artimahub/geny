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
 *  @tparam A the type of element this generator yields. It is covariant, so a
 *    `Generator[Sub]` can be used wherever a `Generator[Super]` is expected. The
 *    methods that change the element type (`map`, `filter`, ...) give up that
 *    covariance because they build a generator of a different type, while the
 *    ones that merely collect (`toBuffer`, `++`) take a second type parameter
 *    bounded below by `A` instead of returning a `Generator[A]`.
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
  /** Runs a side-effecting function on every element in order, draining the
   *  generator. It is [[generate]] with the returned [[Generator.Action]] fixed to
   *  [[Generator.Continue]], so `f` has no way of cutting the traversal short.
   *
   *  @param f applied to each element in turn and its result discarded. `f` has to
   *    cope with everything the generator produces, `null` included if the source
   *    can yield it. An exception thrown by `f` propagates out immediately and the
   *    remaining elements are never enumerated.
   */
  def foreach(f: A => Unit): Unit = generate{ x =>
    f(x)
    Generator.Continue
  }

  /** Finds the first element satisfying a predicate, halting the traversal by
   *  returning [[Generator.End]] as soon as it is found so that later elements
   *  are never evaluated.
   *
   *  @param f the test applied to each element, in order. Because enumeration may
   *    stop at the first match, `f` must be pure and must not rely on being
   *    applied to anything beyond it. An exception thrown by `f` propagates out of
   *    the search.
   *  @return `Some` of the first element for which `f` returned `true`, or `None`
   *    if the generator is exhausted first or holds no matching element. Only the
   *    elements up to and including the match are enumerated.
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
  /** Returns whether at least one element satisfies the predicate, giving up at
   *  the first match just as [[find]] does.
   *
   *  @param f the test applied to each element, in order, until one matches. It must
   *    be pure, since elements after the match are never visited, and an exception
   *    it throws propagates. An empty generator yields `false` without calling `f`
   *    at all.
   */
  def exists(f: A => Boolean) = find(f(_)).isDefined
  /** Returns whether any element is `==` to the given value, checking elements in
   *  order and stopping at the first match.
   *
   *  @param a the value to look for, accepted as `Any`. Each element is compared
   *    against it with `==`, so any type with a sensible `equals` works, `null`
   *    can be searched for as well, and the comparison is null-safe on both sides.
   *    Equality is checked from the element's side, so an asymmetric `equals`
   *    implementation decides the outcome. An empty generator yields `false`.
   */
  def contains(a: Any) = exists(_ == a)
  /** Returns whether the predicate holds for every element, stopping at the first
   *  element that fails it.
   *
   *  @param f the test applied to each element, in order. It must be pure,
   *    because elements after the first failure are never visited, and an exception
   *    it throws propagates. Following the usual reading of the quantifier, an
   *    empty generator returns `true` without ever calling `f`.
   */
  def forall(f: A => Boolean) = !exists(!f(_))
  /** Counts the elements satisfying the predicate, enumerating the generator to
   *  the very end.
   *
   *  @param f the test applied to every element; the default accepts them all, which
   *    makes a bare `count()` the length of the generator. Since nothing is
   *    skipped, `f` must be pure and must cope with every element, and an infinite
   *    generator never terminates. An exception thrown by `f` propagates.
   *  @return the number of elements for which `f` returned `true`.
   */
  def count(f: A => Boolean = ((_: Any) => true)) = {
    var result = 0
    generate{ t =>
      if (f(t)) result +=  1
      Generator.Continue
    }
    result
  }
  /** Folds every element into an accumulator from left to right, starting from
   *  `start`. It is an alias for [[foldLeft]].
   *
   *  @tparam B the type of the accumulator, independent of the element type `A`, so
   *    the caller picks whatever the folding function produces; `f` must return a
   *    `B`.
   *  @param start the initial accumulator, and the result should the generator turn
   *    out to be empty. It is not validated, so a value the elements cannot legally
   *    handle only fails later, inside `f`.
   *  @param f called as `f(accumulatedSoFar, element)` for each element in turn.
   *    For the final value to be meaningful it should be associative, and it must
   *    not throw: an exception escapes this call immediately, abandoning the rest
   *    of the generator.
   *  @return the accumulator after the last element has been folded in, or
   *    `start` unchanged if there were no elements at all.
   */
  def fold[B](start: B)(f: (B, A) => B): B = foldLeft(start)(f)
  /** Folds every element into an accumulator from left to right, returning the
   *  value it ends up with. The whole generator is evaluated.
   *
   *  @tparam B the type of the accumulator, unconstrained by the element type `A`;
   *    `f` has to combine a `B` and an `A` into a `B`.
   *  @param start the value the accumulation starts at, returned untouched for an
   *    empty generator. Nothing checks that it suits the elements, so an unsuitable
   *    `start` fails inside `f` rather than at this call.
   *  @param f applied once per element to the accumulator and the element, in
   *    enumeration order. It should be associative for the result to be meaningful
   *    and must not throw; an exception propagates out and stops the enumeration.
   *  @return the accumulated value once every element has been folded in, which is
   *    `start` itself when the generator is empty.
   */
  def foldLeft[B](start: B)(f: (B, A) => B): B = {
    var result = start
    generate{ t =>
      result = f(result, t)
      Generator.Continue
    }
    result
  }


  /** Combines all the elements into a single value without a seed, taking the
   *  first element as the initial accumulator. It is an alias for [[reduceLeft]].
   *
   *  @tparam B a supertype of the element type `A`. The result is widened to `B`,
   *    which lets a `Generator[Int]` be summed into a `Long` by asking for
   *    `B = Long`; `f` receives that `B` together with the raw `A` element, so it
   *    is responsible for the conversion.
   *  @param f combined with the accumulator built so far and the next element,
   *    once per element after the first. For the answer to be independent of the
   *    element order it should be associative, and it must not throw: an exception
   *    propagates out and abandons the remaining elements.
   *  @return the folded value typed as `B`, or a thrown
   *    `UnsupportedOperationException` if the generator is empty, since there is
   *    no element to seed the reduction with.
   */
  def reduce[B >: A](f: (B, A) => B): B = reduceLeft(f)
  /** Reduces the generator from the left without a seed, using the first element
   *  as the initial value and enumerating the whole thing.
   *
   *  @tparam B a supertype of the element type `A` that the result is widened to;
   *    the accumulator starts life as an `A` and is typed `B` from then on.
   *  @param f combines the accumulator so far with the next element and has to
   *    produce a `B`. It is called exactly once per element beyond the first, never
   *    on an empty generator and never with a seed, so it must not assume one. For
   *    the result to be order-independent `f` should be associative; throwing from
   *    `f` propagates immediately.
   *  @return the accumulated `B`, or a thrown
   *    `UnsupportedOperationException("empty.reduceLeft")` when the generator has no
   *    elements to reduce.
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
  /** Builds a lazy generator over the elements satisfying a predicate, in their
   *  original order.
   *
   *  @param pred the test each element is put to as it passes by. It is re-run on
   *    every enumeration of the result and should be pure; elements that fail it
   *    are never shown to the consumer, but the underlying generator still has to
   *    produce them. A `null` element is passed on as is, so `pred` has to cope
   *    with it.
   *  @return a new `Generator[A]` sharing this generator's laziness and resources.
   *    Nothing is evaluated until the result is enumerated, and enumeration stops
   *    early if the consumer returns [[Generator.End]].
   */
  def filter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Identical to [[filter]]; it exists so that `for` comprehensions with a `val`
   *  guard compile against this trait, and so that such guards can be chained.
   *
   *  @param pred the test applied to each element as it flows through, with the
   *    same requirements as for [[filter]]; in particular it must tolerate `null`
   *    elements if the source can produce them.
   *  @return a lazy `Generator[A]` of the elements `pred` accepted, in order,
   *    evaluated only once the result is enumerated.
   */
  def withFilter(pred: A => Boolean): Generator[A] = new Generator.Filtered(this, pred)
  /** Builds a lazy generator that applies a function to every element as it flows
   *  through.
   *
   *  @tparam B the element type of the resulting generator, unrelated to `A`.
   *  @param func applied to each element, in order, once per element per
   *    enumeration. It is not called when `map` itself is called, so an exception
   *    from `func` only surfaces once the result is enumerated, where it aborts
   *    that enumeration at the offending element.
   *  @return a new lazy `Generator[B]` mapping this generator's elements.
   *    Returning [[Generator.End]] still stops the whole traversal early.
   */
  def map[B](func: A => B): Generator[B] = new Generator.Mapped[B, A](this, func)
  /** Builds a lazy generator by concatenating, for each element in order, all the
   *  elements of the generator that a function maps that element to.
   *
   *  @tparam B the element type of the generators `func` returns, and therefore of
   *    the flattened result.
   *  @param func maps one element to a `Generator[B]`, which is enumerated in full
   *    before the next element of this generator is looked at. It is called
   *    lazily, so a `null` result only fails with `NullPointerException` once
   *    enumeration reaches that element.
   *  @return a lazy `Generator[B]` holding `func(x1)`'s elements, then
   *    `func(x2)`'s, and so on. The inner [[Generator.End]] is what reaches the
   *    outer generator, so a consumer that ends the traversal inside one of the
   *    inner generators also skips the remaining outer elements.
   */
  def flatMap[B](func: A => Generator[B]): Generator[B] = new Generator.FlatMapped[B, A](this, func)
  /** Builds a lazy generator of the results of applying a partial function to the
   *  elements it is defined at, quietly skipping the elements it is not.
   *
   *  @tparam B the result type of the partial function; nothing relates it to `A`.
   *  @param func is applied only to the elements it is defined for. It is used as
   *    `filter(func.isDefinedAt).map(func)`, so `isDefinedAt` and then `apply` are
   *    called separately, once each per element on every enumeration; the two must
   *    agree with each other, and `func` should be pure.
   *  @return a lazy `Generator[B]` of the applied results in order, with the
   *    elements `func` is undefined at omitted.
   */
  def collect[B](func: PartialFunction[A, B]): Generator[B] =
    filter(func.isDefinedAt).map(func)
  /** Applies a partial function to the first element it is defined at and returns
   *  that result, without looking at anything beyond it.
   *
   *  @tparam B the result type of the partial function.
   *  @return `Some` of the result of `func` on the first element it is defined for,
   *    or `None` if no element qualifies or the generator is empty. Implemented as
   *    `filter(...).map(...).headOption`, so enumeration halts at that element and
   *    nothing beyond it is evaluated.
   */
  def collectFirst[B](func: PartialFunction[A, B]): Option[B] =
    filter(func.isDefinedAt).map(func).headOption

  /** Flattens a generator of generators into a single generator of their
   *  elements, relying on an implicit conversion to view each element as a
   *  generator.
   *
   *  @tparam V the element type of the generators the elements are converted into.
   *    It is pinned by the implicit argument at the call site rather than inferred
   *    from a conversion method, so an ambiguous conversion is a compile error.
   *  @param f the implicit conversion applied to each element to obtain a
   *    `Generator[V]`; it is resolved when `flatten` is called, so a missing
   *    conversion fails to compile while an element converting to `null` fails with
   *    `NullPointerException` during enumeration.
   *  @return a lazy `Generator[V]` with the elements of all the converted inner
   *    generators concatenated in order, equivalently `this.flatMap(x => f(x))`.
   */
  def flatten[V](implicit f: A => Generator[V]) = this.flatMap[V]((x: A) => f(x))
  /** Builds a lazy generator over the elements from index `start` up to, but not
   *  including, index `end`.
   *
   *  @param start the index of the first element to yield. Everything before it is
   *    pulled from the underlying generator and thrown away, so skipping a prefix
   *    costs real work on the source; a negative value behaves like `0` and a
   *    value past the end simply yields nothing.
   *  @param end the index to stop before. Enumeration ends as soon as it is reached,
   *    so the element at `end` is never even requested from the source. An `end` of
   *    `start` or less gives an empty generator, and `Int.MaxValue` — what [[drop]]
   *    uses — means no upper bound at all. No bounds checking is performed, so
   *    nonsensical values are accepted quietly.
   *  @return a new lazy `Generator[A]` over the requested half-open range of
   *    indices.
   */
  def slice(start: Int, end: Int): Generator[A] = new Generator.Sliced(this, start, end)
  /** Groups the elements into consecutive non-overlapping chunks of `size`; an
   *  alias for `sliding(size, step = size)`.
   *
   *  @param size the number of elements per chunk. It must be at least one, or the
   *    `require` inside [[sliding]] throws `IllegalArgumentException` right away. A
   *    size larger than the generator is fine and yields one shorter chunk.
   *  @return a lazy `Generator[Iterable[A]]` of chunks in order, the last of which
   *    holds the remainder when the length is not a multiple of `size`. An empty
   *    generator produces no chunks at all, and each chunk is an immutable snapshot
   *    rather than a view of later elements.
   */
  def grouped(size: Int): Generator[Iterable[A]] = sliding(size, step = size)
  /** Builds a lazy generator of overlapping windows of `size` consecutive
   *  elements, advancing the window by `step` elements each time.
   *
   *  @param size the length of each window, which must be at least one. The final
   *    window is shorter when fewer than `size` elements are left, and a generator
   *    holding no elements at all produces no windows.
   *  @param step how far the window start moves between windows; it must be at least
   *    one, and a step larger than `size` skips elements in between. Both arguments
   *    are checked eagerly, so a non-positive value throws
   *    `IllegalArgumentException` at this call rather than during enumeration.
   *  @return a lazy `Generator[Iterable[A]]` of windows in order. Each window is an
   *    immutable snapshot, so a consumer cannot watch later elements through an
   *    earlier one, and a consumer returning [[Generator.End]] suppresses the
   *    trailing partial window.
   */
  def sliding(size: Int, step: Int): Generator[Iterable[A]] = {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")
    new Generator.Sliding(this, size, step)
  }
  /** Builds a lazy generator of at most the first `n` elements, shorthand for
   *  `slice(0, n)`.
   *
   *  @param n the maximum number of elements to keep. Enumeration stops after `n`
   *    elements, which is what makes taking from an infinite generator terminate, so
   *    it has to be finite. A non-positive `n` is not rejected but simply yields an
   *    empty generator.
   */
  def take(n: Int) = slice(0, n)
  /** Builds a lazy generator of everything except the first `n` elements,
   *  shorthand for `slice(n, Int.MaxValue)`.
   *
   *  @param n the number of leading elements to discard. Those elements are still
   *    evaluated by the underlying generator, only their results are thrown away,
   *    so dropping from an infinite generator never finishes. A non-positive `n`
   *    drops nothing.
   */
  def drop(n: Int) = slice(n, Int.MaxValue)
  /** Builds a lazy generator over the longest prefix of elements that all satisfy
   *  the predicate.
   *
   *  @param pred the test applied to each element in order. The first element that
   *    fails it ends the enumeration with [[Generator.End]] and is not part of the
   *    result, so `pred` must be pure and cannot assume any later element will be
   *    examined; a `null` element has to be handled by `pred` itself.
   *  @return a lazy `Generator[A]` of the matching prefix, worked out afresh on
   *    every enumeration.
   */
  def takeWhile(pred: A => Boolean): Generator[A] = new Generator.TakeWhile(this, pred)
  /** Builds a lazy generator that discards the longest prefix satisfying the
   *  predicate and yields everything from the first failing element onwards, that
   *  failing element included.
   *
   *  @param pred the test applied to the leading elements only, until one of them
   *    fails. It must be pure and must tolerate `null` elements; it is not
   *    consulted for anything following the first failure.
   *  @return a lazy `Generator[A]` over the remaining suffix, which is empty when
   *    every element satisfies `pred` and is the whole generator when none does.
   */
  def dropWhile(pred: A => Boolean): Generator[A] = new Generator.DropWhile(this, pred)
  /** Builds a lazy generator pairing every element with its zero-based position in
   *  the enumeration.
   *
   *  The positions count only the elements this generator actually yields, so
   *  zipping after a [[filter]] renumbers the survivors. The counter is bumped
   *  after the handler has run, and only the elements the consumer actually asks
   *  for are ever pulled from the source.
   */
  def zipWithIndex: Generator[(A, Int)] = new Generator.ZipWithIndexed(this)
  /** Pairs every element with the corresponding element of another iterable,
   *  stopping as soon as either side runs out.
   *
   *  @tparam B the element type of `other`, unrelated to `A`.
   *  @param other the second side of the pairs. A fresh `iterator` is taken from it
   *    on every enumeration, and only as many elements as are needed are consumed,
   *    so anything left over is simply never visited and a one-shot `Iterable` is
   *    fine. Passing `null` fails with `NullPointerException` when the result is
   *    enumerated, not when `zip` is called.
   *  @return a lazy `Generator[(A, B)]` that ends as soon as `other` is exhausted,
   *    even if elements remain in this generator.
   */
  def zip[B](other: Iterable[B]): Generator[(A, B)] = new Generator.Zipped(this, other)
  /** Builds a lazy generator holding this generator's elements followed by those of
   *  another one.
   *
   *  @tparam B a supertype of both `A` and the other generator's element type, so
   *    that elements of the two sides can share one generator.
   *  @param other the generator whose elements come second. It is enumerated only
   *    after this one is exhausted, and is not touched at all if the consumer ended
   *    the traversal inside this generator.
   *  @return a lazy `Generator[B]` over the concatenation. It is re-enumerable, so
   *    both sides are traversed again on a second pass.
   */
  def ++[B >: A](other: Generator[B]): Generator[B] = new Generator.Concat(this, other)

  // Conversions
  /** Returns the first element, evaluating only as much of the generator as that
   *  takes.
   *
   *  @return the first element, of type `A`, taken from `take(1).toSeq`. Throws
   *    `NoSuchElementException` if the generator turns out to be empty. Only one
   *    element is ever pulled from the source, so this is safe on an infinite
   *    generator.
   */
  def head = take(1).toSeq.head
  /** Returns the first element if the generator has one, evaluating nothing beyond
   *  it.
   *
   *  @return `Some` of the first element, or `None` for an empty generator; at most
   *    one element is pulled from the source either way. This is the total
   *    counterpart of [[head]].
   */
  def headOption = take(1).toSeq.headOption
  /** Eagerly drains the generator into a mutable buffer, in enumeration order.
   *
   *  @tparam B the element type of the buffer, which has to be a supertype of `A`;
   *    picking a wider one widens every element as it is appended. No `ClassTag` is
   *    needed, since the buffer is not array-backed.
   *  @return a freshly allocated `mutable.Buffer` owned by the caller, holding
   *    every element the generator yielded. Nothing is evaluated before this call,
   *    and everything is evaluated by it.
   */
  def toBuffer[B >: A]: mutable.Buffer[B] = {
    val arr = mutable.Buffer.empty[B]
    foreach{arr.append(_)}
    arr

  }
  /** Eagerly drains the generator into an array, in enumeration order.
   *
   *  @tparam B the element type of the array, a supertype of `A`. The implied
   *    `ClassTag` decides both the array's runtime component type and its
   *    allocation, and it is taken on trust: a tag that does not match the elements
   *    is not diagnosed here and only shows up when the array is used.
   *  @return a new `Array[B]` holding exactly the elements enumerated, obtained
   *    from [[toBuffer]] and hence fresh and not shared with the generator.
   */
  def toArray[B >: A : ClassTag]: Array[B] = toBuffer.toArray
  /** Eagerly materializes the generator as an immutable `Seq`, in enumeration
   *  order, delegating to [[toVector]].
   */
  def toSeq: Seq[A] = toVector
  /** Eagerly materializes the generator as a `List`, in enumeration order,
   *  building it from the intermediate [[toBuffer]].
   */
  def toList = toBuffer.toList
  /** Eagerly materializes the generator as an immutable `Set`, collapsing
   *  duplicate elements.
   *
   *  @tparam B the element type the elements are widened to on their way into the
   *    set; a wider `B` can make previously distinct elements compare equal and
   *    merge into a single entry.
   *  @return an immutable `Set` of the enumerated elements, with no meaningful
   *    iteration order.
   */
  def toSet[B >: A] = toBuffer[B].toSet
  /** Eagerly materializes the generator as a `Vector`, in enumeration order,
   *  converting the intermediate [[toBuffer]].
   */
  def toVector = toBuffer.toVector
  /** Renders the elements as text, with an affix at each end and a separator
   *  between consecutive elements, draining the generator in the process.
   *
   *  @param start the text placed before the first element.
   *  @param sep inserted only between elements and never at either end, so an
   *    empty generator renders as `start + end`, a single-element one as
   *    `start + element + end`, and an empty `sep` runs them all together.
   *  @param end the text placed after the last element.
   *  @return the concatenated `String`. Elements go in through
   *    `StringBuilder.append`, which uses each element's `toString` and renders a
   *    `null` element as the text `null`.
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
  /** Renders the elements separated by the given text, with no affixes at either
   *  end; shorthand for `mkString("", sep, "")`.
   *
   *  @param sep the text placed between consecutive elements and nowhere else.
   *  @return the concatenated `String`, which is `""` for an empty generator.
   */
  def mkString(sep: String): String = mkString("", sep, "")
  /** Renders all the elements back-to-back, with neither separator nor affixes, in
   *  enumeration order.
   */
  def mkString: String = mkString("")


  /** Adds up every element, enumerating the generator in full.
   *
   *  @tparam B the supertype of `A` the addition is carried out in and the sum is
   *    returned as, so a `Generator[Int]` can be summed into a `Long` by asking for
   *    `B = Long`. A `Numeric[B]` has to be available for that type.
   *  @param num the implicit `Numeric` providing `zero` as the seed and `plus` for
   *    each step, resolved from `B` at the call site; there is no fallback, so a type
   *    without such an instance does not compile.
   *  @return the sum as a `B`, which is `num.zero` for an empty generator.
   */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Multiplies together every element, enumerating the generator in full.
   *
   *  @tparam B the supertype of `A` the multiplication is carried out in and the
   *    product is returned as.
   *  @param num the implicit `Numeric` providing `one` as the seed and `times` for
   *    each step, resolved from `B` at the call site; there is no fallback, so a
   *    type without such an instance does not compile.
   *  @return the product as a `B`, which is `num.one` for an empty generator.
   */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Returns the smallest element according to an ordering, by reducing the whole
   *  generator.
   *
   *  @tparam B the supertype of `A` the elements are compared as; the winner is
   *    handed back as the original element of type `A`.
   *  @param cmp the implicit `Ordering` used for the `<=` test. Since the comparison
   *    is not strict, the earliest of several equal minima wins, and an ordering
   *    that is not a total order gives an unspecified result.
   *  @return the minimal element as an `A`, or a thrown
   *    `UnsupportedOperationException("empty.reduceLeft")` for an empty generator,
   *    since it reduces rather than defaulting.
   */
  def min[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  /** Returns the largest element according to an ordering, by reducing the whole
   *  generator.
   *
   *  @tparam B the supertype of `A` the elements are compared as; the winner is
   *    handed back as the original element of type `A`.
   *  @param cmp the implicit `Ordering` used for the `>=` test. Since the comparison
   *    is not strict, the earliest of several equal maxima wins, and an ordering
   *    that is not a total order gives an unspecified result.
   *  @return the maximal element as an `A`, or a thrown
   *    `UnsupportedOperationException("empty.reduceLeft")` for an empty generator,
   *    since it reduces rather than defaulting.
   */
  def max[B >: A](implicit cmp: Ordering[B]): A = {
    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }

  /** Returns the element whose extracted key is the largest, comparing the keys of
   *  every element in the generator.
   *
   *  @tparam B the type of the key `f` produces, unrelated to the element type.
   *  @param f maps an element to the key to compare. It is evaluated once per
   *    element on every call, and never applied to the `null` placeholder the
   *    implementation starts out with, since the first element is taken
   *    unconditionally; an exception it throws propagates.
   *  @param cmp the implicit `Ordering` of the keys, applied with a strict `>` so
   *    that among equal keys the earliest element is kept.
   *  @return the element with the greatest key, as an `A`. Unlike [[min]] this does
   *    not reduce, so an empty generator has no first element to fall back on and
   *    yields `null` rather than throwing.
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
  /** Returns the element whose extracted key is the smallest, comparing the keys of
   *  every element in the generator.
   *
   *  @tparam B the type of the key `f` produces, unrelated to the element type.
   *  @param f maps an element to the key to compare. It is evaluated once per
   *    element on every call, and never applied to the `null` placeholder the
   *    implementation starts out with, since the first element is taken
   *    unconditionally; an exception it throws propagates.
   *  @param cmp the implicit `Ordering` of the keys, applied with a strict `<` so
   *    that among equal keys the earliest element is kept.
   *  @return the element with the smallest key, as an `A`. Unlike [[max]] this does
   *    not reduce, so an empty generator has no first element to fall back on and
   *    yields `null` rather than throwing.
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
  /** The control-flow verdict handed back from the handler passed to
   *  [[Generator.generate]], telling the generator whether to keep pulling
   *  elements or to stop. The trait is sealed, so [[Generator.Continue]] and
   *  [[Generator.End]] are the only two possible values, and the implementations
   *  recognise them by reference equality.
   */
  sealed trait Action
  object End extends Action
  object Continue extends Action


  /** Builds a [[Generator]] over the given elements.
   *
   *  @tparam T the element type, inferred from the arguments; calling `Generator`
   *    with no arguments at all infers `Nothing` and gives an empty generator.
   *  @param xs the elements, in order. They are collected into a `Seq` as `apply`
   *    is called, so later mutation of a caller-supplied array is not reflected
   *    here, while the resulting generator itself can be enumerated as often as
   *    desired.
   */
  def apply[T](xs: T*) = from(xs)

  /** Implicitly turns a container into a lazy [[Generator]] over its elements.
   *
   *  @tparam M the higher-kinded container type of `t`, such as `Seq` or
   *    `IndexedSeq`. Naming it explicitly is what lets `Generator.from` infer the
   *    element type of sources that are too precise to infer from.
   *  @tparam T the element type.
   *  @param t the container to wrap. It is not touched here: `toString` merely
   *    records it, and the conversion happens on each enumeration.
   *  @param convert an implicit `M[T] => TraversableOnce[T]` that turns the
   *    container into something with an `iterator`. Without one in scope the call
   *    does not compile, and a conversion returning `null` fails with
   *    `NullPointerException` when the generator is enumerated.
   *  @return a `Generator[T]` that takes a fresh iterator from `convert(t)` on every
   *    `generate` call, so it can be re-enumerable as long as `t` is; wrapping a
   *    one-shot `TraversableOnce` makes a second enumeration yield nothing. Its
   *    `toString` is `Generator(<t>)`.
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


  /** Builds a [[Generator]] from a lazily produced iterator plus a cleanup
   *  callback that is guaranteed to run once enumeration is over, however it
   *  ends.
   *
   *  @tparam T the element type the iterator produces.
   *  @param makeIterator a by-name factory, evaluated once at the start of every
   *    `generate` call and expected to return the `Iterator[T]` to pull from
   *    together with a zero-argument function to run afterwards. Nothing about it
   *    is checked: if the factory itself throws, the callback was never obtained
   *    and so never runs.
   *  @return a `Generator[T]` whose `generate` invokes the callback from a `finally`
   *    block, so it runs exactly once whether enumeration completes normally, is cut
   *    short by [[Generator.End]], or propagates an exception thrown by the
   *    consumer.
   */
  def selfClosing[T](makeIterator: => (Iterator[T], () => Unit)): Generator[T] = new SelfClosing(makeIterator)

  private class SelfClosing[+T](makeIterator: => (Iterator[T], () => Unit)) extends Generator[T]{
    /** Makes the iterator and drains it, making sure the cleanup callback runs
     *  whatever happens.
     *
     *  @param f the handler invoked with each element; the [[Generator.Action]] it
     *    returns is honoured, so returning [[Generator.End]] stops the loop without
     *    the iterator having been exhausted. An exception it throws propagates once
     *    the cleanup callback has run.
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
    /** The constant string `Gen.SelfClosing(...)`; the by-name iterator factory is
     *  deliberately left unexpanded, since expanding it would acquire the resource
     *  this class exists to guard.
     */
    override def toString = s"Gen.SelfClosing(...)"
  }



  private class Concat[+T](inner: Generator[T], other: Generator[T]) extends Generator[T] {
    /** Enumerates the first generator and then the second one, unless the consumer
     *  ended the traversal inside the first.
     *
     *  @param f the handler, passed on unchanged to both generators. The
     *    [[Generator.Action]] returned for an element of the first generator decides
     *    whether the second one is enumerated at all, and an `End` returned from
     *    within the second propagates back out.
     */
    def generate(f: T => Generator.Action) = {
      val res1 = inner.generate(f)
      if (res1 == Generator.End) Generator.End
      else other.generate(f)

    }
    /** Renders as `first ++ second`, interpolating both generators' own
     *  `toString`s, so nesting shows up in full.
     */
    override def toString = s"$inner ++ $other"
  }

  private class ZipWithIndexed[+T](inner: Generator[T]) extends Generator[(T, Int)] {
    /** Enumerates the underlying generator, handing each element to the consumer
     *  alongside a running counter.
     *
     *  @param f called as `f(element, index)` with `index` starting at `0`. The
     *    counter is bumped after the call returns, so an exception from `f` leaves it
     *    at the index of the failing element. Returning [[Generator.End]] stops the
     *    underlying generator as usual.
     */
    def generate(f: ((T, Int)) => Generator.Action) = {
      var i = 0
      inner.generate{t =>
        val res = f(t, i)
        i += 1
        res
      }
    }
    /** Renders as `inner.zipWithIndex`, recursing into the underlying generator's
     *  `toString`.
     */
    override def toString = s"$inner.zipWithIndex"
  }

  private class Zipped[+T, V](inner: Generator[T], other: Iterable[V]) extends Generator[(T, V)] {
    /** Walks the underlying generator alongside an iterator over `other`, pairing
     *  the two up and stopping as soon as `other` runs dry.
     *
     *  @param f called as `f(element, otherElement)` for each pair. Once the `other`
     *    iterator is exhausted the handler is not called at all and
     *    [[Generator.End]] is returned instead, which costs one element pulled from
     *    the underlying generator and then discarded.
     */
    def generate(f: ((T, V)) => Generator.Action) = {
      val iter = other.iterator
      inner.generate{t =>
        if (!iter.hasNext) Generator.End
        else f(t, iter.next())
      }
    }
    /** Renders as `inner.zip(other)`, interpolating both sides' `toString`s. */
    override def toString = s"$inner.zip($other)"
  }

  private class Filtered[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Passes the elements satisfying the predicate on to the consumer and
     *  swallows the rest.
     *
     *  @param f the consumer's handler, called only for the elements `pred`
     *    accepted. Rejected elements still come out of the underlying generator and
     *    simply produce [[Generator.Continue]] here, and whatever `f` returns for an
     *    accepted element is what the underlying generator sees.
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => if (pred(t)) f(t) else Generator.Continue}
    }
    /** Renders as `inner.filter(predicate)`, showing the predicate as its own
     *  `toString`.
     */
    override def toString = s"$inner.filter($pred)"
  }

  private class Mapped[+T, V](inner: Generator[V], func: V => T) extends Generator[T]{
    /** Applies the mapping function to each element on its way through.
     *
     *  @param f the consumer's handler, called with `func(element)` rather than with
     *    the element itself, and only once the underlying generator has produced it.
     *    An exception from `func` propagates and aborts the enumeration.
     */
    def generate(f: T => Generator.Action) = {
      inner.generate{t => f(func(t))}
    }
    /** Renders as `inner.map(function)`, which shows the function as its own
     *  `toString` and so normally as some flavour of lambda text.
     */
    override def toString = s"$inner.map($func)"
  }

  private class FlatMapped[+T, V](inner: Generator[V], func: V => Generator[T]) extends Generator[T]{
    /** Enumerates the inner generator belonging to each element in turn, passing
     *  everything it yields on to the consumer.
     *
     *  @param f the consumer's handler, called once per element of the inner
     *    generators. The [[Generator.Action]] it returns travels back out through
     *    the inner generator to the outer one, so returning [[Generator.End]]
     *    anywhere halts the whole flattened traversal rather than just the current
     *    inner generator.
     */
    def generate(f: T => Generator.Action) = {

      inner.generate{ outerT =>
        func(outerT).generate{ innerT =>
          f(innerT)
        }
      }
    }
    /** Renders as `inner.map(function)`: the label says `map` even though this is a
     *  `flatMap`, and the function is shown as its own `toString`.
     */
    override def toString = s"$inner.map($func)"
  }

  private class Sliding[+T](inner: Generator[T], size: Int, step: Int) extends Generator[Iterable[T]] {
    /** Buffers the underlying generator's elements into windows, holding a running
     *  buffer of `max(size, step)` elements to build each window out of.
     *
     *  @param f the consumer's handler, called once per full window of `size`
     *    elements, plus once more at the end for the trailing partial window if
     *    there is one. Every argument is an immutable snapshot of the buffered
     *    elements, so a window never changes after it has been emitted. Returning
     *    [[Generator.End]] stops the underlying generator and suppresses that final
     *    partial window.
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
    /** Counts its way through the underlying generator, discarding everything
     *  before `start` and stopping right after `end`.
     *
     *  @param f the consumer's handler, called for each element whose index falls in
     *    the half-open range `[start, end)`. The element at `end - 1` is still passed
     *    to `f`, but it is followed straight away by [[Generator.End]] instead of by
     *    a request for one more element, so the underlying generator is never asked
     *    for the element at `end`.
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
    /** Renders as `inner.slice(start, end)`. */
    override def toString = s"$inner.slice($start, $end)"
  }

  private class TakeWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Passes elements on to the consumer for as long as they satisfy the
     *  predicate, then ends the enumeration.
     *
     *  @param f the consumer's handler, called only for elements `pred` accepted. The
     *    first element that fails is not passed on: it produces
     *    [[Generator.End]], which stops the underlying generator, and whatever `f`
     *    returned for the previous element is what comes back out.
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
    /** Renders as `inner.takeWhile(predicate)`. */
    override def toString = s"$inner.takeWhile($pred)"
  }

  private class DropWhile[+T](inner: Generator[T], pred: T => Boolean) extends Generator[T]{
    /** Swallows the leading elements that satisfy the predicate and forwards
     *  everything from the first failure onwards.
     *
     *  @param f the consumer's handler. It is first called for the very element that
     *    failed `pred` — that element belongs to the result — and for each element
     *    after it; `pred` is not consulted again once the prefix has been dropped.
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
    /** Renders as `inner.dropWhile(predicate)`. */
    override def toString = s"$inner.dropWhile($pred)"
  }

}
