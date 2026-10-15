package geny

import java.nio.charset.StandardCharsets

import scala.io.Codec

/**
 * Encapsulates an `Array[Byte]` and provides convenience methods for
 * reading the data out of it.
 */
trait ByteData {

  /** The raw bytes held by this [[ByteData]]. */
  def bytes: Array[Byte]

  /** Decodes [[bytes]] to a `String` as UTF-8 by delegating to [[text]]. */
  def text(): String = text(StandardCharsets.UTF_8)
  /** Decodes [[bytes]] into a `String` using the given codec's character set.
   *
   *  @param codec the codec whose `charSet` is used to decode [[bytes]]; must not be null,
   *  otherwise a `NullPointerException` is thrown when its character set is read
   *  @return the decoded `String`, empty when [[bytes]] is empty
   */
  def text(codec: Codec): String = new String(bytes, codec.charSet)

  /** Decodes [[bytes]] as UTF-8 and strips leading and trailing whitespace by delegating to [[trim]]. */
  def trim(): String = trim(StandardCharsets.UTF_8)
  /** Decodes [[bytes]] with the given codec and strips leading and trailing whitespace.
   *
   *  @param codec the codec whose `charSet` is used to decode [[bytes]] before trimming; must not be null,
   *  otherwise a `NullPointerException` is thrown when its character set is read
   *  @return the decoded text with leading and trailing characters at or below `U+0020` removed
   */
  def trim(codec: Codec): String = text(codec).trim

  /** Splits [[bytes]] decoded as UTF-8 into lines by delegating to [[lines]]. */
  def lines(): Vector[String] = lines(StandardCharsets.UTF_8)
  /** Decodes [[bytes]] with the given codec and splits the text into lines without terminators.
   *
   *  @param codec the codec whose `charSet` is used to decode [[bytes]] before splitting; must not be null,
   *  otherwise a `NullPointerException` is thrown when its character set is read
   *  @return a `Vector` with one entry per line without line terminators, empty when the decoded text is empty
   */
  def lines(codec: Codec): Vector[String] = Predef.augmentString(text(codec)).lines.toVector
}
object ByteData{
  /** A [[ByteData]] that concatenates a sequence of [[Bytes]] chunks in order.
   *
   *  @param chunks the [[Bytes]] elements concatenated in iteration order to form [[bytes]];
   *  must not be null and must contain no nulls, otherwise access to [[bytes]] throws
   *  a `NullPointerException`, while an empty sequence yields an empty array
   */
  case class Chunks(chunks: Seq[Bytes]) extends ByteData{
    /** Lazily concatenates the `array` of each [[Bytes]] in [[chunks]] into a single array, computed once. */
    lazy val bytes = chunks.iterator.map(_.array).toArray.flatten
  }
}