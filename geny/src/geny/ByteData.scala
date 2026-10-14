package geny

import java.nio.charset.StandardCharsets

import scala.io.Codec

/**
 * Encapsulates an `Array[Byte]` and provides convenience methods for
 * reading the data out of it.
 */
trait ByteData {

  /** The raw bytes held by this data. */
  def bytes: Array[Byte]

  /** Decodes the bytes as a UTF-8 string. */
  def text(): String = text(StandardCharsets.UTF_8)
  /** Decodes the bytes as a string using the given codec.
   *
   *  @param codec the character encoding used to decode the bytes
   *  @return the decoded string
   */
  def text(codec: Codec): String = new String(bytes, codec.charSet)

  /** Decodes the bytes as UTF-8 and removes leading and trailing whitespace. */
  def trim(): String = trim(StandardCharsets.UTF_8)
  /** Decodes the bytes using the given codec and removes leading and trailing whitespace.
   *
   *  @param codec the character encoding used to decode the bytes
   *  @return the trimmed decoded string
   */
  def trim(codec: Codec): String = text(codec).trim

  /** Decodes the bytes as UTF-8 and splits the result into lines. */
  def lines(): Vector[String] = lines(StandardCharsets.UTF_8)
  /** Decodes the bytes using the given codec and splits the result into lines.
   *
   *  @param codec the character encoding used to decode the bytes
   *  @return the lines of the decoded text
   */
  def lines(codec: Codec): Vector[String] = Predef.augmentString(text(codec)).lines.toVector
}
object ByteData{
  /** A [[ByteData]] whose content is the concatenation of a sequence of byte chunks.
   *
   *  @param chunks the byte chunks making up the data, in order
   */
  case class Chunks(chunks: Seq[Bytes]) extends ByteData{
    /** The concatenation of all chunks as a single byte array. */
    lazy val bytes = chunks.iterator.map(_.array).toArray.flatten
  }
}