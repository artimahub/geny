package geny

import java.nio.charset.StandardCharsets

import scala.io.Codec

/**
 * Encapsulates an `Array[Byte]` and provides convenience methods for
 * reading the data out of it.
 */
trait ByteData {

  /** TODO FILL IN */
  def bytes: Array[Byte]

  /** TODO FILL IN */
  def text(): String = text(StandardCharsets.UTF_8)
  /** TODO FILL IN
   *
   *  @param codec TODO FILL IN
   *  @return TODO FILL IN
   */
  def text(codec: Codec): String = new String(bytes, codec.charSet)

  /** TODO FILL IN */
  def trim(): String = trim(StandardCharsets.UTF_8)
  /** TODO FILL IN
   *
   *  @param codec TODO FILL IN
   *  @return TODO FILL IN
   */
  def trim(codec: Codec): String = text(codec).trim

  /** TODO FILL IN */
  def lines(): Vector[String] = lines(StandardCharsets.UTF_8)
  /** TODO FILL IN
   *
   *  @param codec TODO FILL IN
   *  @return TODO FILL IN
   */
  def lines(codec: Codec): Vector[String] = Predef.augmentString(text(codec)).lines.toVector
}
object ByteData{
  /** TODO FILL IN
   *
   *  @param chunks TODO FILL IN
   */
  case class Chunks(chunks: Seq[Bytes]) extends ByteData{
    /** TODO FILL IN */
    lazy val bytes = chunks.iterator.map(_.array).toArray.flatten
  }
}