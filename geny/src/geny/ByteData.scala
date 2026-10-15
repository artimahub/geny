package geny

import java.nio.charset.StandardCharsets

import scala.io.Codec

/**
 * Encapsulates an `Array[Byte]` and provides convenience methods for
 * reading the data out of it.
 */
trait ByteData {

  /**
   * The raw octets backing this [[ByteData]], with no encoding or
   * transformation applied. Implementations may cache and hand back the same
   * array instance on every call, so callers that intend to modify the array
   * must copy it first.
   *
   *  @return the bytes of this [[ByteData]], in order
   */
  def bytes: Array[Byte]

  /**
   * Decodes [[bytes]] as UTF-8. Byte sequences that are not valid UTF-8 are
   * replaced with the Unicode replacement character rather than raising an
   * error.
   */
  def text(): String = text(StandardCharsets.UTF_8)
  /**
   * Decodes [[bytes]] using the charset carried by the given `Codec`,
   * substituting that charset's replacement characters for any input it cannot
   * decode.
   *
   *  @param codec the codec whose `charSet` performs the decoding; nothing else
   *               on the `Codec` is consulted, so its encoder, decoder and
   *               error actions have no effect here. Passing `null` throws a
   *               `NullPointerException`.
   *  @return the decoded string, empty if [[bytes]] is empty. A successful
   *          return does not mean the bytes were valid in that encoding:
   *          undecodable sequences show up as replacement characters.
   */
  def text(codec: Codec): String = new String(bytes, codec.charSet)

  /**
   * Decodes [[bytes]] as UTF-8 and strips leading and trailing characters
   * whose code point is at or below `U+0020`, following `String.trim`.
   */
  def trim(): String = trim(StandardCharsets.UTF_8)
  /**
   * Decodes [[bytes]] using the given `Codec` and then strips leading and
   * trailing characters whose code point is at or below `U+0020`. Trimming
   * happens after decoding, so multi-byte characters such as `U+00A0` are
   * left in place.
   *
   *  @param codec the codec whose `charSet` performs the decoding, as in
   *               `text(codec)`; passing `null` throws a `NullPointerException`.
   *  @return the decoded string without its leading and trailing
   *          whitespace-as-`U+0020`; a payload made up entirely of such
   *          characters, or an empty one, returns the empty string.
   */
  def trim(codec: Codec): String = text(codec).trim

  /**
   * Decodes [[bytes]] as UTF-8 and splits the result into lines. A trailing
   * line terminator does not contribute a final empty line, so an empty
   * payload yields an empty result.
   */
  def lines(): Vector[String] = lines(StandardCharsets.UTF_8)
  /**
   * Decodes [[bytes]] using the given `Codec` and splits the decoded text on
   * line terminators, which are `\n`, `\r\n` and `\r`; the terminators
   * themselves are not part of the returned lines.
   *
   *  @param codec the codec whose `charSet` performs the decoding, as in
   *               `text(codec)`; passing `null` throws a `NullPointerException`.
   *  @return the lines in order. A trailing terminator does not add an empty
   *          last line, but a payload of just `\n` returns a single empty
   *          line, and an empty payload returns an empty `Vector`.
   */
  def lines(codec: Codec): Vector[String] = Predef.augmentString(text(codec)).lines.toVector
}
object ByteData{
  /**
   * A [[ByteData]] assembled by concatenating several [[Bytes]] instances, in
   * iteration order.
   *
   *  @param chunks the [[Bytes]] to join, in the order they should appear in
   *                [[bytes]]. The join happens lazily, so constructing a
   *                `Chunks` copies nothing; an empty sequence decodes to an
   *                empty string. A `null` element in the sequence throws a
   *                `NullPointerException` when [[bytes]] is first computed.
   */
  case class Chunks(chunks: Seq[Bytes]) extends ByteData{
    /**
     * The `array` of every [[Bytes]] in [[chunks]], concatenated in order.
     * Computed once on first access and then cached, and returned as a fresh
     * array, so mutating it leaves the underlying [[Bytes]] untouched.
     */
    lazy val bytes = chunks.iterator.map(_.array).toArray.flatten
  }
}