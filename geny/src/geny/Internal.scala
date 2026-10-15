package geny

import java.io.{InputStream, OutputStream}
import java.lang.Character.MAX_SURROGATE
import java.lang.Character.MIN_SURROGATE

/** The buffer-sizing defaults and byte-handling helpers shared by the
 *  [[Writable]] implementations: the constants that size the transfer buffer
 *  of `transfer0`, the `transfer` built on top of it, and `encodedLength` for
 *  measuring the UTF-8 length of a `String`.
 */
object Internal {
  /** The default ceiling for the transfer buffer of [[transfer0]] and
   *  [[transfer]]: 65536 bytes.
   *
   *  Despite the "StartSize" in the name this is not only the size the buffer
   *  starts at, it is the largest the buffer ever becomes. It defaults the
   *  `maxBufferSize` parameter, so a stream that claims to have a gigabyte
   *  `available` still starts with 65536 bytes and is copied through buffers of
   *  that size at most.
   */
  val defaultMaxBufferStartSize: Int = 64 * 1024
  /** The default floor for the transfer buffer of [[transfer0]] and
   *  [[transfer]]: 64 bytes.
   *
   *  It defaults the `minBufferSize` parameter, which is what keeps a stream
   *  reporting `available() == 0` from producing a zero-length buffer. A
   *  zero-length buffer matters because `InputStream.read(Array[Byte])` is
   *  specified to return 0 rather than -1 for one, and [[transfer0]] loops
   *  until it sees -1, so an empty buffer would spin forever.
   */
  val defaultMinBufferStartSize: Int = 64
  /** The default growth threshold for the transfer buffer of [[transfer0]] and
   *  [[transfer]]: the buffer is only doubled once the number of bytes
   *  transferred so far is strictly greater than four times its current length.
   *
   *  It defaults the `bufferGrowthRatio` parameter. A transfer therefore never
   *  reallocates for data smaller than the buffer it already holds: a buffer
   *  that starts at 64 bytes survives a 256 byte transfer untouched, and first
   *  grows on the 257th byte.
   */
  val defaultBufferGrowthRatio: Int = 4

  /**
   * Transfers data from the given `InputStream` through a `sink` function
   * through a dynamically sized transfer buffer.
   *
   * The buffer is sized based on the `.available` number on the input stream,
   * clamped to between `minBufferSize` and `maxBufferSize`, and allowed to
   * grow in increments of 2x if the total amount of bytes transferred exceeds
   * the size of the buffer by `bufferGrowthRatio`.
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

  /** Copies every byte still to be read from `src` into `dest` with
   *  [[transfer0]], using `dest.write(chunk, 0, bytesRead)` as the sink so that
   *  each read turns into exactly one write call.
   *
   *  Nothing is buffered beyond the single transfer buffer, so the copy streams
   *  and neither stream has to hold the whole payload at once. `src` is closed
   *  before this returns and must not be used afterwards; `dest` is left open
   *  and unflushed, so closing and flushing it stays the caller's job.
   *
   *  @param src the stream to drain. It is read until `read` returns -1 and then
   *             closed. Only closed on the success path: if `src.read` or a
   *             `dest.write` throws, the exception propagates and `src` is left
   *             open, since the close is not guarded by a `finally`.
   *  @param dest the stream that receives the bytes, in order. A `null` here
   *              fails with `NullPointerException` on the first write.
   *  @param minBufferSize the smallest transfer buffer to allocate, used as the
   *                       floor when `src.available` reports less. Defaults to
   *                       [[defaultMinBufferStartSize]]. Passing 0 lets a stream
   *                       with `available() == 0` produce a zero-length buffer,
   *                       which never makes progress and loops forever.
   *  @param maxBufferSize the largest transfer buffer to allocate; growth stops
   *                       here. Defaults to [[defaultMaxBufferStartSize]].
   *                       Nothing is validated, so a `maxBufferSize` below
   *                       `minBufferSize` silently leaves `minBufferSize` in
   *                       force.
   *  @param bufferGrowthRatio how many buffer-lengths of transferred data must
   *                           accumulate before the buffer is doubled.
   *                           Defaults to [[defaultBufferGrowthRatio]].
   *  @return `()`, meaning success by reaching the end of `src`. There is no
   *          return value to inspect: a truncated or failed copy surfaces as an
   *          `IOException` from `src` or `dest` rather than as a short value.
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
  /** Computes how many bytes `sequence` will occupy when encoded as UTF-8,
   *  without actually encoding it, so a payload can be given a content length
   *  before the bytes exist. Vendored from Guava's `Utf8.encodedLength` and used
   *  here by the string-backed [[Writable]]s for their `contentLength`.
   *
   *  The result equals `sequence.getBytes(UTF_8).length` for any string this
   *  accepts: ASCII costs 1 byte per char, chars below `0x800` cost 2, and the
   *  rest cost 3, with a well formed surrogate pair counting as 4 for its two
   *  chars. Chars are examined as UTF-16 code units, so a lone surrogate is
   *  detected rather than measured.
   *
   *  @param sequence the string to measure. It is read through `length` and
   *                  `charAt`, so `null` raises `NullPointerException` instead of
   *                  measuring 0. A string containing an unpaired surrogate —
   *                  a high surrogate with no low surrogate after it, or a stray
   *                  low surrogate — cannot be encoded as UTF-8 and raises
   *                  `IllegalArgumentException` with the index of the offending
   *                  char. `""` is accepted and measures 0.
   *  @return the UTF-8 length in bytes, always between 0 and
   *          `Int.MaxValue`. It is a plain `Int`, not an `Option`, so callers
   *          get the number or an exception; a length that would overflow an
   *          `Int` (roughly a billion non-ASCII chars, or a two billion char
   *          ASCII string) raises `IllegalArgumentException` reporting
   *          `utf8Length + 2^32` rather than the true length.
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
