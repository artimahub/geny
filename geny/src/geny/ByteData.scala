package geny

import java.nio.charset.StandardCharsets

import scala.io.Codec

/**
 * Encapsulates an `Array[Byte]` and provides convenience methods for
 * reading the data out of it.
 */
trait ByteData {

  /** The raw bytes that this [[ByteData]] wraps. Every other member of the
   *  trait is defined in terms of this one, so it is the only method an
   *  implementation has to supply.
   */
  def bytes: Array[Byte]

  /** Decodes [[bytes]] as UTF-8 text. */
  def text(): String = text(StandardCharsets.UTF_8)
  /** Decodes [[bytes]] as text using the character encoding of the given codec.
   *
   *  @param codec the codec whose `charSet` names the encoding to decode with; a
   *    `java.nio.charset.Charset` such as `StandardCharsets.UTF_8` may be passed
   *    directly and is converted implicitly. Only the charset is consulted, so
   *    passing a codec for the wrong encoding silently yields mojibake, while a
   *    codec that is not charset-based throws `UnsupportedOperationException`
   *    from `charSet`.
   *  @return the decoded string, one character per decoded byte sequence. Byte
   *    sequences that are malformed under the codec's encoding are replaced by
   *    the encoding's replacement character rather than raising an error, and
   *    [[bytes]] is left untouched.
   */
  def text(codec: Codec): String = new String(bytes, codec.charSet)

  /** Decodes [[bytes]] as UTF-8 text and strips whitespace from both ends,
   *  exactly as `java.lang.String.trim` does: only characters at or below
   *  U+0020 are removed, so non-ASCII whitespace is left in place.
   */
  def trim(): String = trim(StandardCharsets.UTF_8)
  /** Decodes [[bytes]] with the given codec and strips whitespace from both ends.
   *
   *  @param codec the codec whose `charSet` names the encoding to decode with,
   *    exactly as for [[text(codec)]]: a `Charset` is accepted through an implicit
   *    conversion, a wrong encoding produces mojibake, and a codec that is not
   *    charset-based throws `UnsupportedOperationException`.
   *  @return the decoded string with leading and trailing characters at or below
   *    U+0020 removed, using the rules of `java.lang.String.trim`. Whitespace
   *    and line breaks in the middle of the value are untouched, and the result
   *    is empty if the decoded text was nothing but such characters.
   */
  def trim(codec: Codec): String = text(codec).trim

  /** Decodes [[bytes]] as UTF-8 text and splits it into lines, dropping the
   *  terminator that ends the final line.
   */
  def lines(): Vector[String] = lines(StandardCharsets.UTF_8)
  /** Decodes [[bytes]] with the given codec and splits it into lines.
   *
   *  @param codec the codec whose `charSet` names the encoding to decode with,
   *    exactly as for [[text(codec)]]: a `Charset` is accepted through an implicit
   *    conversion, a wrong encoding produces mojibake, and a codec that is not
   *    charset-based throws `UnsupportedOperationException`.
   *  @return a `Vector` of the decoded lines, in order, without their terminators.
   *    Lines are split at `\n`, `\r\n` and `\r`; a terminator at the very end of
   *    the text does not produce a final empty element, while empty bytes yield
   *    the single empty line `Vector("")`.
   */
  def lines(codec: Codec): Vector[String] = Predef.augmentString(text(codec)).lines.toVector
}
object ByteData{
  /** A [[ByteData]] whose bytes are the concatenation of several [[Bytes]],
   *  without copying them all up front.
   *
   *  @param chunks the pieces to join, in the order they should appear in the
   *    resulting bytes. An empty `Seq` gives a `ByteData` of length zero; a
   *    `null` `Seq` or a `null` element throws `NullPointerException` once the
   *    bytes are actually needed.
   */
  case class Chunks(chunks: Seq[Bytes]) extends ByteData{
    /** The `array` of every element of [[chunks]] laid end to end, copied into
     *  a freshly allocated array of the combined length.
     *
     *  Because this is a `lazy val`, the concatenation runs once per `Chunks`
     *  instance, on first access; later replacement of the `Seq` or mutation of
     *  the underlying arrays is not reflected in the cached result, and the
     *  returned array is never one of the arrays held by the [[Bytes]].
     */
    lazy val bytes = chunks.iterator.map(_.array).toArray.flatten
  }
}