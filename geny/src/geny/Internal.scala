package geny

import java.io.{InputStream, OutputStream}
import java.lang.Character.MAX_SURROGATE
import java.lang.Character.MIN_SURROGATE

object Internal {
  /** Default for the `maxBufferSize` argument of [[Internal.transfer0]] and
   *  [[Internal.transfer]]: 65536 bytes, the hard ceiling on the transfer
   *  buffer. It clamps the initial size derived from `src.available`, so a
   *  stream reporting more than 64 KiB available still starts from a 64 KiB
   *  buffer, and it also stops the buffer from doubling past that size, which
   *  bounds the allocation however much is transferred.
   */
  val defaultMaxBufferStartSize: Int = 64 * 1024
  /** Default for the `minBufferSize` argument of [[Internal.transfer0]] and
   *  [[Internal.transfer]]: 64 bytes, the floor the transfer buffer is clamped
   *  to. It only ever binds on the initial, `available`-derived size, since
   *  growth always doubles a buffer that already respects it. It is what keeps
   *  a stream reporting `available` as 0 out of getting a zero-length buffer,
   *  whose `read` would return 0 forever; passing 0 or less makes the transfer
   *  loop never terminate.
   */
  val defaultMinBufferStartSize: Int = 64
  /** Default for the `bufferGrowthRatio` argument of [[Internal.transfer0]] and
   *  [[Internal.transfer]]: 4, meaning the buffer is only doubled once the
   *  total number of bytes transferred so far exceeds four times its current
   *  length. A short payload therefore never pays for a reallocation, and each
   *  doubling is amortized over more than four bufferfuls of copying. A ratio
   *  of 1 or less doubles the buffer after every non-empty read instead.
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

  /** Copies the whole of `src` into `dest` over the dynamically sized buffer of
   *  [[Internal.transfer0]], writing each chunk as it is read. `src` is closed
   *  once it has been drained, but only on that path: an exception thrown by
   *  either `read` or `write` propagates out and leaves `src` unclosed. `dest`
   *  is never flushed or closed, which remains the caller's responsibility. The
   *  three sizing arguments are handed to [[Internal.transfer0]] unchanged, so
   *  their defaults and the growth rules described there apply here as well.
   *
   *  A `null` `src` throws `NullPointerException` straight away, when its
   *  `available` is queried, whereas a `null` `dest` only does so once the
   *  first chunk arrives: an empty `src` writes nothing and never notices it.
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
  /** Returns the number of bytes `sequence` occupies once encoded as UTF-8,
   *  counting one byte per char below `0x80`, two per char below `0x800` and
   *  three above that, except a well-formed surrogate pair, which counts four
   *  bytes for the pair. For a well-formed string this is the length
   *  `String.getBytes(StandardCharsets.UTF_8)` produces, which is what
   *  [[Writable.contentLength]] reports through it.
   *
   *  @param sequence the string to measure, expected to be well-formed. A
   *    `null` string throws `NullPointerException`, and an unpaired surrogate
   *    throws `IllegalArgumentException` naming its index, because such a
   *    string has no single well-defined encoding length.
   *  @return the length of `sequence` in UTF-8 bytes, at most three per char. A
   *    length too large for an `Int` — more than about 715 million chars — is
   *    reported by throwing `IllegalArgumentException` rather than returned as
   *    a number that has wrapped around into a negative value.
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
