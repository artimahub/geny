package geny

/** Trivial wrapper around `Array[Byte]` with sane equality and useful toString
 *
 *  @param array the backing byte array stored by reference without copying;
 *               callers are expected to pass a non-null array they no longer
 *               mutate, and passing `null` leaves later operations to the
 *               underlying JDK calls, so content comparison treats `null`
 *               as `java.util.Arrays.equals` does and string conversion throws
 *               `NullPointerException`
 */
class Bytes(val array: Array[Byte]){
  /** Compares this [[Bytes]] with another value by the contents of the backing arrays.
   *
   *  @param other any value to compare against; only a [[Bytes]] instance can
   *               match, so `null` or any other type yields `false` without throwing
   *  @return `true` if `other` is a [[Bytes]] whose array holds exactly the
   *          same bytes in the same order as this instance, otherwise `false`
   */
  override def equals(other: Any) = other match{
    case otherBytes: Bytes => java.util.Arrays.equals(array, otherBytes.array)
    case _ => false
  }
  /** Decodes the backing array as a UTF-8 string.
   *
   *  @return the decoded `String`, with malformed or unmappable input decoded
   *          with replacement rather than throwing; throws `NullPointerException`
   *          if the backing array is `null`
   */
  override def toString = new String(array, java.nio.charset.StandardCharsets.UTF_8)
}