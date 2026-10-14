package geny

/** Trivial wrapper around `Array[Byte]` with sane equality and useful toString
 *
 *  @param array the wrapped byte array
 */
class Bytes(val array: Array[Byte]){
  /** Compares the wrapped arrays by content; returns false for non-`Bytes` values.
   *
   *  @param other the object to compare with this `Bytes`
   */
  override def equals(other: Any) = other match{
    case otherBytes: Bytes => java.util.Arrays.equals(array, otherBytes.array)
    case _ => false
  }
  /** Decodes the wrapped bytes as a UTF-8 string. */
  override def toString = new String(array, java.nio.charset.StandardCharsets.UTF_8)
}