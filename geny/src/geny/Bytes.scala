package geny

/** Trivial wrapper around `Array[Byte]` with sane equality and useful toString
 *
 *  @param array TODO FILL IN
 */
class Bytes(val array: Array[Byte]){
  /** TODO FILL IN
   *
   *  @param other TODO FILL IN
   */
  override def equals(other: Any) = other match{
    case otherBytes: Bytes => java.util.Arrays.equals(array, otherBytes.array)
    case _ => false
  }
  /** TODO FILL IN */
  override def toString = new String(array, java.nio.charset.StandardCharsets.UTF_8)
}