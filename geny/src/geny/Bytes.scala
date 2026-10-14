package geny

/** Trivial wrapper around `Array[Byte]` with sane equality and useful toString
 *
 *  @param array the bytes this wrapper stands for. The array is stored as given
 *    and never copied, so the caller keeps a handle on the live data: mutating it
 *    afterwards is immediately visible through [[array]], [[equals]] and
 *    [[toString]], and because equality compares contents rather than identity,
 *    such a mutation can change the outcome of a comparison that was made
 *    earlier. A `null` array is accepted without complaint at construction time,
 *    though [[toString]] then throws `NullPointerException`.
 */
class Bytes(val array: Array[Byte]){
  /** Two [[Bytes]] are equal when they wrap arrays holding the same bytes in the
   *  same order, and are unequal to every other value.
   *
   *  Note that `hashCode` is *not* overridden alongside this method, so the two
   *  disagree: equal wrappers may hash differently, which makes a [[Bytes]]
   *  behave by identity inside hashed collections such as `java.util.HashSet`.
   *
   *  @param other the value to compare against, normally another [[Bytes]]. Any
   *    other value, including `null`, yields `false`; nothing is thrown, so this
   *    is safe to call on untrusted input.
   *  @return `true` if `other` is a [[Bytes]] whose `array` has the same length
   *    as [[array]] and holds equal values in every position, `false`
   *    otherwise. As a consequence of delegating to
   *    `java.util.Arrays.equals`, two wrappers over `null` arrays compare equal
   *    to each other, while a wrapper over a `null` array compares unequal to
   *    one over a real array.
   */
  override def equals(other: Any) = other match{
    case otherBytes: Bytes => java.util.Arrays.equals(array, otherBytes.array)
    case _ => false
  }
  /** The wrapped bytes decoded as UTF-8 text. Sequences that are not valid UTF-8
   *  become the Unicode replacement character `U+FFFD` rather than raising an
   *  error, so no exception is thrown for malformed input; a wrapper over a
   *  `null` array does throw `NullPointerException`.
   */
  override def toString = new String(array, java.nio.charset.StandardCharsets.UTF_8)
}