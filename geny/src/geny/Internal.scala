package geny

import java.io.{InputStream, OutputStream}
import java.lang.Character.MAX_SURROGATE
import java.lang.Character.MIN_SURROGATE

object Internal {
  /** Default upper bound in bytes for the transfer buffer used by [[transfer0]] and [[transfer]].
    *
    * [[transfer0]] clamps its initial and doubled buffer sizes to at most this value, which is 64 KiB.
    */
  val defaultMaxBufferStartSize: Int = 64 * 1024
  /** Default lower bound in bytes for the transfer buffer used by [[transfer0]] and [[transfer]].
    *
    * [[transfer0]] clamps its initial and doubled buffer sizes to at least this value, which is 64 bytes.
    */
  val defaultMinBufferStartSize: Int = 64
  /** Default growth threshold multiplier used by [[transfer0]] and [[transfer]].
    *
    * [[transfer0]] doubles its buffer once the total bytes transferred exceed the current buffer length times this ratio.
    */
  val defaultBufferGrowthRatio: Int = 4

  /**
   * Transfers data from the given `InputStream` through a to a [[sink]]
   * function through a dynamically sized transfer buffer.
   *
   * The buffer is sized based on the `.available` number on the input stream,
   * clamped a [[minBufferSize]] and [[maxBufferSize]] size, and allowed to
   * grow in increments of 2x if the total amount of bytes transferred exceeds
   * the size of the buffer by [[bufferGrowthRatio]].
   *
   * This should allow efficient processing of `InputStream`s of all sizes,
   * from the tiny to the very large, without over- or under- allocating the
   * size of the transfer buffer.
   */
  def transfer0(src: InputStream,
                sink: (Array[Byte], Int) => Unit,
                minBufferSize: Int = defaultMinBufferStartSize,
                maxBufferSize: Int = defaultMaxBufferStartSize,
                bufferGrowthRatio: Int = defaultBufferGrowthRatio) = {
    def clampBufferSize(n: Int) = {
      if (n < minBufferSize) minBufferSize
      else if (n > maxBufferSize) maxBufferSize
      else n
    }

    var buffer = new Array[Byte](clampBufferSize(src.available()))
    var total = 0
    var read = 0
    while (read != -1) {
      read = src.read(buffer)
      if (read != -1) {
        sink(buffer, read)
        total += read
        if (total > buffer.length * bufferGrowthRatio && buffer.length < maxBufferSize) {
          buffer = new Array[Byte](clampBufferSize(buffer.length * 2))
        }
      }
    }
    src.close()
  }

  /**
    * Copies all remaining bytes from `src` to `dest` by delegating to [[transfer0]].
    *
    * Reads `src` to end with a dynamically sized buffer and writes each chunk to `dest`,
    * then closes `src` while leaving `dest` open.
    *
    *  @param src the open `InputStream` to read from; must be non-null, otherwise a `NullPointerException` is thrown, and a closed or failing stream throws `IOException` from `available`, `read`, or `close`.
    *  @param dest the open `OutputStream` receiving each chunk via `write(buffer, 0, read)`; must be non-null and open, otherwise a `NullPointerException` or `IOException` is thrown when the first chunk is written, and it is never closed here.
    *  @param minBufferSize the minimum initial and grown buffer size in bytes; should be non-negative and `<= maxBufferSize`, otherwise the clamp in [[transfer0]] can request a negatively sized or oversized array and throw.
    *  @param maxBufferSize the maximum buffer size in bytes that growth never exceeds; should be non-negative and `>= minBufferSize`, otherwise sizing falls back to `minBufferSize` without further growth.
    *  @param bufferGrowthRatio the multiplier controlling growth in [[transfer0]]; the buffer doubles once total bytes transferred exceed `buffer.length * bufferGrowthRatio`, so a non-positive value causes growth after the first chunk up to `maxBufferSize`.
    */
  def transfer(src: InputStream,
               dest: OutputStream,
               minBufferSize: Int = defaultMinBufferStartSize,
               maxBufferSize: Int = defaultMaxBufferStartSize,
               bufferGrowthRatio: Int = defaultBufferGrowthRatio) = transfer0(
    src,
    dest.write(_, 0, _),
    minBufferSize,
    maxBufferSize,
    bufferGrowthRatio
  )

  // VENDORED FROM GUAVA
  /** Returns the number of bytes needed to encode `sequence` as UTF-8 without performing the encoding.
   *
   * Counts ASCII as one byte, `U+0080` to `U+07FF` as two bytes, BMP characters at or above `U+0800` as three bytes, and each well-formed surrogate pair as four bytes.
   *  @param sequence the `String` whose UTF-8 length is computed; must be non-null and contain only well-formed surrogate pairs, so `null` throws `NullPointerException` and an unpaired surrogate throws `IllegalArgumentException`.
   *  @return the UTF-8 encoded length in bytes; throws `IllegalArgumentException` when the length does not fit in an `Int`.
   */
  def encodedLength(sequence: String) = { // Warning to maintainers: this implementation is highly optimized.
    val utf16Length = sequence.length
    var utf8Length = utf16Length
    var i = 0
    // This loop optimizes for pure ASCII.
    while (i < utf16Length && sequence.charAt(i) < 0x80) i += 1
    // This loop optimizes for chars less than 0x800.

    while (i < utf16Length) {
      val c = sequence.charAt(i)
      if (c < 0x800) utf8Length += ((0x7f - c) >>> 31) // branch free!
      else {
        utf8Length += encodedLengthGeneral(sequence, i)
        i = utf16Length // break is not supported, just set i to terminate the loop

      }

      i += 1
    }
    if (utf8Length < utf16Length) { // Necessary and sufficient condition for overflow because of maximum 3x expansion
      throw new IllegalArgumentException("UTF-8 length does not fit in int: " + (utf8Length + (1L << 32)))
    }
    utf8Length
  }

  private def encodedLengthGeneral(sequence: String, start: Int) = {
    val utf16Length = sequence.length
    var utf8Length = 0
    var i = start
    while (i < utf16Length) {
      val c = sequence.charAt(i)
      if (c < 0x800) utf8Length += (0x7f - c) >>> 31
      else {
        utf8Length += 2
        // jdk7+: if (Character.isSurrogate(c)) {
        if (MIN_SURROGATE <= c && c <= MAX_SURROGATE) { // Check that we have a well-formed surrogate pair.
          if (Character.codePointAt(sequence, i) == c) throw new IllegalArgumentException(unpairedSurrogateMsg(i))
          i += 1
        }
      }

      i += 1
    }
    utf8Length
  }

  private def unpairedSurrogateMsg(i: Int) = "Unpaired surrogate at index " + i
  // END VENDORED FROM GUAVA
}
