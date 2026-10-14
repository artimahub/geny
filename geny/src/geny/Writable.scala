package geny
import java.io.{ByteArrayInputStream, InputStream, OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

import sun.nio.cs.StreamEncoder

/**
 * A [[Writable]] is a source of bytes that can be written to an OutputStream.
 *
 * Essentially a push-based version of `java.io.InputStream`, that allows an
 * implementation to guarantee that cleanup logic runs after the bytes are
 * written.
 *
 * [[Writable]] is also much easier to implement than `java.io.InputStream`: any
 * code that previously wrote output to an `ByteArrayOutputStream` or
 * `StringBuilder` can trivially satisfy the [[Writable]] interface. That makes
 * [[Writable]] very convenient to use for allowing zero-friction zero-overhead
 * streaming data exchange between different libraries.
 *
 * [[Writable]] comes with implicit constructors from `Array[Byte]`, `String`
 * and `InputStream`, and is itself a tiny interface with minimal functionality.
 * Libraries using [[Writable]] are expected to extend it to provide additional
 * methods or additional implicit constructors that make sense in their context.
 */
trait Writable extends Any{
  /** Writes every byte this [[Writable]] holds to `out`, and returns once the
   *  last of them has been handed over to the stream.
   *
   *  Implementations are expected to leave `out` open and unflushed: flushing
   *  or closing a stream belongs to whoever opened it, so that a single stream
   *  can receive the bytes of several writables in turn.
   *
   *  @param out the stream to write to. A `null` stream throws
   *    `NullPointerException` as soon as there is something to write; a
   *    writable that has nothing to write may never touch `out` at all.
   */
  def writeBytesTo(out: OutputStream): Unit
  /** The value to announce for the HTTP `Content-Type` header, for callers
   *  sending this content over the wire. The default implementation returns
   *  `None`, which means no opinion rather than "no header": the type is
   *  unknown, and the caller decides to omit the header or fill it in.
   */
  def httpContentType: Option[String] = None
  /** The exact number of bytes [[Writable.writeBytesTo]] will write, for
   *  callers that would rather send a `Content-Length` than buffer the content
   *  or send it chunked.
   *
   *  The default implementation returns `None`, meaning the length is not known
   *  without actually producing the bytes. Implementations that do know it
   *  override this, and the answer must then stay in agreement with what
   *  `writeBytesTo` writes.
   */
  def contentLength: Option[Long] = None
}
object Writable extends LowPriWritable {
  /** Wraps a `String` as a [[Writable]] that emits its UTF-8 encoding.
   *
   *  The conversion is implicit, so a `String` can be handed to anything that
   *  asks for a [[Writable]] without an explicit wrapping call.
   *
   *  @param s the string to write. A `null` string throws
   *    `NullPointerException`. An unpaired surrogate is replaced by `?` while
   *    encoding, which is why [[StringWritable.contentLength]] — measured by
   *    [[Internal.encodedLength]] — rejects such a string instead.
   */
  implicit class StringWritable(s: String) extends Writable{
    /** Encodes `s` as UTF-8 and writes it to `out` in chunks of up to 8192
     *  characters, one `write` call per chunk, leaving the stream open and
     *  unflushed. An empty string produces no chunk, and therefore no write
     *  at all.
     *
     *  @param out the stream to write to. A `null` stream throws
     *        `NullPointerException` unless `s` is empty, in which case `out`
     *        is never dereferenced.
     */
    def writeBytesTo(out: OutputStream): Unit = {

      s.grouped(8192).foreach(ss => out.write(ss.getBytes(StandardCharsets.UTF_8)))
    }
    /** Always `Some("text/plain; charset=utf-8")`, naming the encoding
     *  [[StringWritable.writeBytesTo]] actually applies, so that a client
     *  decodes the bytes back into the same characters.
     */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** `Some` with the number of bytes the UTF-8 encoding of `s` occupies,
     *  obtained from [[Internal.encodedLength]] without allocating the encoded
     *  array. Throws `IllegalArgumentException` for an unpaired surrogate,
     *  which has no single well-defined encoded length.
     */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Wraps an `Array[Byte]` as a [[Writable]] that hands those bytes to the
   *  stream unencoded.
   *
   *  The conversion is implicit, so an `Array[Byte]` can be handed to anything
   *  that asks for a [[Writable]] without an explicit wrapping call.
   *
   *  @param a the bytes to write. The array is neither copied nor consumed, so
   *    whatever it holds at the moment [[ByteArrayWritable.writeBytesTo]] runs
   *    is what gets written, and it can be written more than once. A `null`
   *    array throws `NullPointerException`.
   */
  implicit class ByteArrayWritable(a: Array[Byte]) extends Writable{
    /** Writes the whole array to `out` with a single `out.write(a)` call,
     *  leaving the stream open and unflushed.
     *
     *  @param out the stream to write to. A `null` stream throws
     *    `NullPointerException`, as does a `null` array.
     */
    def writeBytesTo(out: OutputStream): Unit = out.write(a)
    /** Always `Some("application/octet-stream")`: raw bytes carry no encoding
     *  of their own, so no charset is announced alongside the type.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some` with the array's length, which is exactly what
     *  [[ByteArrayWritable.writeBytesTo]] writes — as long as the array is
     *  not resized between the two calls.
     */
    override def contentLength = Some(a.length)
  }

  /** Wraps a `java.nio.ByteBuffer` as a [[Writable]] that emits the bytes
   *  between the buffer's current `position` and its `limit`.
   *
   *  The conversion is implicit, so a `ByteBuffer` can be handed to anything
   *  that asks for a [[Writable]] without an explicit wrapping call.
   *
   *  @param buffer the buffer to read from. Only `buffer.remaining()` bytes are
   *    written; the buffer's own position, limit and byte order are left
   *    untouched, so the same buffer can be written repeatedly. A `null`
   *    buffer throws `NullPointerException`.
   */
  implicit class ByteBufferWritable(buffer: java.nio.ByteBuffer) extends Writable {
    /** Copies the buffer's remaining bytes through an 8 KiB scratch array and
     *  writes one chunk at a time to `out`, leaving the stream open and
     *  unflushed.
     *
     *  The buffer is duplicated before being drained, so this neither consumes
     *  nor reorders the buffer it was given, and a buffer with no remaining
     *  bytes writes nothing at all.
     *
     *  @param out the stream to write to. A `null` stream throws
     *        `NullPointerException` unless the buffer has nothing remaining,
     *        in which case `out` is never dereferenced.
     */
    def writeBytesTo(out: OutputStream): Unit = {
      // TODO: there is room for optimization here. We could match on the output
      // stream and in case it has an underlying NIO channel, write the buffer
      // directly to it. E.g.
      //
      // out match {
      //   case fs: java.io.FileOutputStream => fs.getChannel().write(buffer)
      //   case _ =>
      // }
      //
      // This optimization however is not available on ScalaJS, and hence
      // requires a restructuring of this source file.
      val bb = buffer.duplicate().order(buffer.order())
      var tmp = new Array[Byte](8192)
      val length = bb.remaining()
      var count = 0
      while (count < length) {
        val l = math.min(tmp.size, length - count)
        bb.get(tmp, 0, l)
        out.write(tmp, 0, l)
        count += l
      }
    }
    /** Always `Some("application/octet-stream")`: the bytes come straight out
     *  of the buffer, with no encoding of their own to announce.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some` with the number of bytes between the buffer's current position
     *  and its limit, widened to `Long` — the same count
     *  [[ByteBufferWritable.writeBytesTo]] writes, since it never moves the
     *  buffer, unless the caller changes the buffer in between the calls.
     */
    override def contentLength = Some(buffer.remaining())
  }

}

/** Mixes [[LowPriWritable.readableWritable]] into the low-priority part of
 *  the implicit scope.
 *
 *  The companions offering specific [[Writable]] instances extend this trait,
 *  so that the generic `T => Readable` conversion sits below them: a `String`,
 *  an `Array[Byte]` or a `ByteBuffer` then resolves to its own direct
 *  [[Writable]] instead of raising an ambiguity between two applicable
 *  implicit conversions.
 */
trait LowPriWritable{
  /** Converts any value that can already be turned into a [[Readable]] into a
   *  [[Writable]], by handing the value to that conversion and keeping its
   *  result.
   *
   *  @tparam T the type of the value being converted. It is unconstrained
   *    apart from having an implicit `T => Readable` conversion available,
   *    which is what makes this conversion apply at all.
   *  @param t the value to convert.
   *  @param f the implicit conversion from `T` to [[Readable]]. If no such
   *    conversion is in scope, Scala does not apply this one at all; if
   *    several are in scope equally well, Scala reports an ambiguous implicit
   *    conversion rather than letting this method pick one.
   *  @return the [[Readable]] `t` converts to, viewed as a [[Writable]], so
   *    writing it hands out the same bytes.
   */
  implicit def readableWritable[T](t: T)(implicit f: T => Readable): Writable = f(t)
}

/**
 * A [[Readable]] is a source of bytes that can be read from an InputStream
 *
 * A subtype of [[Writable]], every [[Readable]] can be trivially used as a
 * [[Writable]] by transferring the bytes from the InputStream to the OutputStream,
 * but not every [[Writable]] is a [[Readable]].
 *
 * Note that the InputStream is only available inside the `readBytesThrough`, and
 * may be closed and cleaned up (along with any associated resources) once the
 * callback returns.
 */
trait Readable extends Writable{
  /** Hands a stream over this [[Readable]]'s bytes to `f`, and returns
   *  whatever `f` returns.
   *
   *  The stream is only guaranteed to be readable for the duration of the
   *  call: an implementation may close it, and whatever resource sits behind
   *  it, as soon as `f` returns, so it must not be used afterwards.
   *
   *  @tparam T the result type of `f`. It fixes the type of this call, which
   *    is how a callback that just reads and returns `Unit` makes the whole
   *    call return `Unit`.
   *  @param f the function that consumes the stream.
   *  @return the value `f` returned, passed through untouched. An exception
   *    thrown by `f` or by reading the stream propagates to the caller, with
   *    no value produced.
   */
  def readBytesThrough[T](f: InputStream => T): T
  /** Transfers the whole content to `out` using [[Internal.transfer]], which
   *  closes the `InputStream` once it has been drained. `out` itself is left
   *  open and unflushed; if reading or writing throws, the exception
   *  propagates and the stream is left unclosed.
   *
   *  @param out the stream to write to. A `null` stream throws
   *    `NullPointerException` once the first chunk arrives, unless there is
   *    nothing to write, in which case it is never noticed.
   */
  def writeBytesTo(out: OutputStream): Unit = readBytesThrough(Internal.transfer(_, out))
}
object Readable{
  /** Wraps a `String` as a [[Readable]] over its UTF-8 encoding.
   *
   *  The conversion is implicit, so a `String` can be handed to anything that
   *  asks for a [[Readable]] without an explicit wrapping call.
   *
   *  @param s the string to read. A `null` string throws
   *    `NullPointerException`. An unpaired surrogate is replaced by `?` while
   *    encoding, which is why [[StringReadable.contentLength]] — measured by
   *    [[Internal.encodedLength]] — rejects such a string instead.
   */
  implicit class StringReadable(s: String) extends Readable{
    /** Encodes `s` as UTF-8, wraps the resulting bytes in a fresh
     *  `ByteArrayInputStream` and hands it to `f`. The encoding happens on
     *  every call, and the stream is backed by a private copy of the bytes
     *  that is dropped along with the stream once `f` returns.
     *
     *  @tparam T the result type of `f`, returned unchanged by this call.
     *  @param f the function to hand the stream to. It may read the stream
     *    once, partly, or not at all; it is not closed on its behalf.
     *  @return the value `f` returned; an exception thrown by `f` propagates
     *    to the caller.
     */
    def readBytesThrough[T](f: InputStream => T): T = {
      f(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    }
    /** Always `Some("text/plain; charset=utf-8")`, naming the encoding
     *  [[StringReadable.readBytesThrough]] actually applies.
     */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** `Some` with the number of bytes the UTF-8 encoding of `s` occupies,
     *  obtained from [[Internal.encodedLength]] without encoding the string.
     *  Throws `IllegalArgumentException` for an unpaired surrogate, which has
     *  no single well-defined encoded length.
     */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Wraps an `Array[Byte]` as a [[Readable]] over those bytes.
   *
   *  The conversion is implicit, so an `Array[Byte]` can be handed to anything
   *  that asks for a [[Readable]] without an explicit wrapping call.
   *
   *  @param a the bytes to read. The array is neither copied nor consumed; the
   *    stream wraps it directly, so mutating it while `f` is running changes
   *    what `f` sees, and re-reading after `f` has drained the stream starts
   *    over from the beginning. A `null` array throws `NullPointerException`.
   */
  implicit class ByteArrayReadable(a: Array[Byte]) extends Readable{
    /** Wraps the array in a fresh `ByteArrayInputStream` and hands it to `f`,
     *  without copying the array itself.
     *
     *  @tparam T the result type of `f`, returned unchanged by this call.
     *  @param f the function to hand the stream to. It may read the stream
     *    once, partly, or not at all; it is not closed on its behalf.
     *  @return the value `f` returned; an exception thrown by `f` propagates
     *    to the caller.
     */
    def readBytesThrough[T](f: InputStream => T): T = f(new ByteArrayInputStream(a))
    /** Always `Some("application/octet-stream")`: raw bytes carry no encoding
     *  of their own, so no charset is announced alongside the type.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some` with the array's length, which is what reading the array through
     *  yields — as long as the array is not resized in between.
     */
    override def contentLength = Some(a.length)
  }

  /** Wraps a `java.nio.ByteBuffer` as a [[Readable]] over the bytes between the
   *  buffer's current `position` and its `limit`.
   *
   *  The conversion is implicit, so a `ByteBuffer` can be handed to anything
   *  that asks for a [[Readable]] without an explicit wrapping call.
   *
   *  @param buffer the buffer to read from. Only `buffer.remaining()` bytes are
   *    read, and the buffer is duplicated before anything is taken out of it,
   *    so its position, limit and byte order survive the read untouched and
   *    the same buffer can be read again. A `null` buffer throws
   *    `NullPointerException`.
   */
  implicit class ByteBufferReadable(buffer: java.nio.ByteBuffer) extends Readable{
    /** Hands `f` a fresh `InputStream` that pulls single bytes or byte ranges
     *  out of a duplicate of `buffer`, and returns whatever `f` returns.
     *
     *  The stream reports `-1` once the buffer's remaining bytes run out, and
     *  neither closes nor retains the buffer, so `f` must read everything it
     *  needs before it returns.
     *
     *  @tparam T the result type of `f`, returned unchanged by this call.
     *  @param f the function to hand the stream to. It is not closed on its
     *    behalf, and must not be retained past its own return.
     *  @return the value `f` returned; an exception thrown by `f` propagates
     *    to the caller, leaving the buffer as it was.
     */
    def readBytesThrough[T](f: InputStream => T): T = {
      val bb = buffer.duplicate().order(buffer.order())

      val is = new InputStream {
        override def read(): Int = if (!bb.hasRemaining()) {
          -1
        } else {
          bb.get() & 0xff
        }
        override def read(bytes: Array[Byte], off: Int, len: Int) = if (!bb.hasRemaining()) {
          -1
        } else {
          val l = math.min(len, bb.remaining())
          bb.get(bytes, off, l)
          l
        }
      }

      f(is)
    }
    /** Always `Some("application/octet-stream")`: the bytes come straight out
     *  of the buffer, with no encoding of their own to announce.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some` with the number of bytes between the buffer's current position
     *  and its limit, widened to `Long` — the same count
     *  [[ByteBufferReadable.readBytesThrough]] can hand out, since it never
     *  moves the buffer, unless the caller changes the buffer in between.
     */
    override def contentLength = Some(buffer.remaining())
  }

  /** Wraps an `InputStream` as a [[Readable]] over that very stream.
   *
   *  The conversion is implicit, so an `InputStream` can be handed to anything
   *  that asks for a [[Readable]] without an explicit wrapping call.
   *
   *  @param i the stream to read. It is neither buffered nor copied, so it can
   *    be drained only once, its `available` is queried while transferring
   *    (see [[Readable.writeBytesTo]]), and closing it inside the callback
   *    closes the caller's stream. A `null` stream throws
   *    `NullPointerException`.
   */
  implicit class InputStreamReadable(i: InputStream) extends Readable{
    /** Hands `f` the wrapped stream itself, without copying, buffering or
     *  wrapping it, and returns whatever `f` returns.
     *
     *  Cleanup is `f`'s responsibility, not this method's: a `f` that closes
     *  the stream closes the caller's stream, and one that returns without
     *  draining it leaves the remainder for the caller to deal with.
     *
     *  @tparam T the result type of `f`, returned unchanged by this call.
     *  @param f the function to hand the stream to. It receives the caller's
     *    own stream and is responsible for closing it if that is wanted.
     *  @return the value `f` returned; an exception thrown by `f` propagates
     *    to the caller with the stream in whatever state `f` left it.
     */
    def readBytesThrough[T](f: InputStream => T): T = f(i)
    /** Always `Some("application/octet-stream")`: the bytes are whatever the
     *  wrapped stream produces, with no encoding of their own to announce.
     *  Note that [[Readable.contentLength]] keeps its inherited `None`
     *  here, since a stream can only report its length by reading.
     */
    override def httpContentType: Option[String] = Some("application/octet-stream")
  }
}
