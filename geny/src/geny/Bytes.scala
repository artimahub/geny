package geny

/** Trivial wrapper around `Array[Byte]` with sane equality and useful toString
 *
 *  @param array the bytes this instance stands for. The array is held by
 *               reference rather than copied, and it is public, so a caller
 *               that mutates it afterwards changes what [[toString]] renders
 *               and what [[equals]] compares. Nothing is validated here: a
 *               `null` array is accepted and only fails later, with the
 *               `NullPointerException` described on [[toString]].
 */
class Bytes(val array: Array[Byte]){
  /** Compares the wrapped bytes by content, so two distinct arrays holding
   *  the same octets are equal.
   *
   *  @param other the value to compare against. Only a [[Bytes]] can ever be
   *               equal to `this`; every other value, including `null`, falls
   *               through to `false` instead of raising.
   *  @return `true` when `other` is a [[Bytes]] whose array has the same
   *          length and equal elements at every index, as decided by
   *          `java.util.Arrays.equals`. Arrays of differing lengths are not
   *          equal even when one is a prefix of the other. Note that `hashCode`
   *          is not overridden, so the inherited identity hash does not agree
   *          with this `equals`.
   */
  override def equals(other: Any) = other match{
    case otherBytes: Bytes => java.util.Arrays.equals(array, otherBytes.array)
    case _ => false
  }
  /** Renders the wrapped bytes by decoding them as UTF-8.
   *
   *  @return the decoded string. Input that is not valid UTF-8 is replaced by
   *          the Unicode replacement character rather than raising, so the
   *          result is a lossy view of [[array]]: distinct byte sequences can
   *          print as the same string. If [[array]] is `null` this throws
   *          `NullPointerException` rather than returning a placeholder.
   */
  override def toString = new String(array, java.nio.charset.StandardCharsets.UTF_8)
}