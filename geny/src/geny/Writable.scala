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
  /** Writes all bytes of this [[Writable]] to `out`, leaving `out` open.
   *
   *  @param out the `OutputStream` receiving every byte; must be non-null and open,
   *  otherwise a `NullPointerException` or `IOException` is thrown from the write.
   */
  def writeBytesTo(out: OutputStream): Unit
  /** Returns the HTTP `Content-Type` for this [[Writable]], or `None` when unknown.
   *
   *  The base implementation returns `None`.
   */
  def httpContentType: Option[String] = None
  /** Returns the exact byte length of this [[Writable]] when known, otherwise `None`.
   *
   *  The base implementation returns `None`.
   */
  def contentLength: Option[Long] = None
}
object Writable extends LowPriWritable {
  /** Adapts a `String` to [[Writable]] by encoding it as UTF-8 bytes.
   *
   *  @param s the `String` whose UTF-8 bytes are written; must be non-null,
   *  otherwise encoding throws a `NullPointerException` when the bytes are written or measured.
   */
  implicit class StringWritable(s: String) extends Writable{
    /** Writes `s` to `out` as UTF-8 bytes in chunks of 8192 characters, leaving `out` open.
     *
     *  @param out the `OutputStream` receiving the encoded bytes; must be non-null and open,
     *  otherwise a `NullPointerException` or `IOException` is thrown after a partial write.
     */
    def writeBytesTo(out: OutputStream): Unit = {

      s.grouped(8192).foreach(ss => out.write(ss.getBytes(StandardCharsets.UTF_8)))
    }
    /** Returns `Some("text/plain; charset=utf-8")` for the UTF-8 encoding written by [[StringWritable]]. */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** Returns `Some` of the UTF-8 byte length of `s`, computed by [[Internal.encodedLength]] without encoding. */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Adapts an `Array[Byte]` to [[Writable]] by writing its full contents.
   *
   *  @param a the array whose bytes are written; must be non-null,
   *  otherwise `writeBytesTo` throws a `NullPointerException`, and concurrent mutation of `a` is visible to the write.
   */
  implicit class ByteArrayWritable(a: Array[Byte]) extends Writable{
    /** Writes the whole contents of `a` to `out` in one `write` call, leaving `out` open.
     *
     *  @param out the `OutputStream` receiving `a.length` bytes; must be non-null and open,
     *  otherwise a `NullPointerException` or `IOException` is thrown.
     */
    def writeBytesTo(out: OutputStream): Unit = out.write(a)
    /** Returns `Some("application/octet-stream")`, as raw bytes carry no text type. */
    override def httpContentType = Some("application/octet-stream")
    /** Returns `Some(a.length)`, the exact number of bytes `writeBytesTo` writes. */
    override def contentLength = Some(a.length)
  }

  /** Adapts a `java.nio.ByteBuffer` to [[Writable]] over its remaining bytes.
   *
   *  @param buffer the buffer whose `remaining` bytes are written; must be non-null,
   *  otherwise `writeBytesTo` and `contentLength` throw a `NullPointerException`.
   */
  implicit class ByteBufferWritable(buffer: java.nio.ByteBuffer) extends Writable {
    /** Writes the `remaining` bytes of a duplicate of `buffer` to `out` in 8192-byte chunks, leaving the original position unchanged and `out` open.
     *
     *  @param out the `OutputStream` receiving `buffer.remaining()` bytes; must be non-null and open,
     *  otherwise a `NullPointerException` or `IOException` is thrown after a partial write.
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
    /** Returns `Some("application/octet-stream")`, as raw bytes carry no text type. */
    override def httpContentType = Some("application/octet-stream")
    /** Returns `Some(buffer.remaining())`, the number of bytes `writeBytesTo` writes at call time. */
    override def contentLength = Some(buffer.remaining())
  }

}

/** Holds the low-priority implicit conversion from [[Readable]] to [[Writable]]. */
trait LowPriWritable{
  /** Converts any `T` viewable as [[Readable]] into a [[Writable]].
   *
   *  @tparam T the source type that can be viewed as [[Readable]].
   *  @param t the value to convert, passed unchanged to `f`.
   *  @param f the implicit view from `T` to [[Readable]]; must not be missing or return `null`,
   *  otherwise implicit resolution fails or the resulting [[Writable]] throws a `NullPointerException` when used.
   *  @return `f(t)` as a [[Writable]], supplying exactly the bytes of the underlying [[Readable]].
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
  /** Exposes the bytes as an `InputStream` for the duration of `f` and returns its result.
   *
   *  @tparam T the result type produced by `f`.
   *  @param f the callback receiving the open `InputStream`; must be non-null and must not retain or use the stream after it returns,
   *  since the stream may be closed and cleaned up once `f` returns.
   *  @return the value returned by `f`; any exception from opening the stream or from `f` propagates to the caller.
   */
  def readBytesThrough[T](f: InputStream => T): T
  /** Copies all bytes from [[readBytesThrough]] to `out` via [[Internal.transfer]], closing the source stream and leaving `out` open.
   *
   *  @param out the `OutputStream` receiving every byte; must be non-null and open,
   *  otherwise a `NullPointerException` or `IOException` is thrown after the source stream is closed.
   */
  def writeBytesTo(out: OutputStream): Unit = readBytesThrough(Internal.transfer(_, out))
}
object Readable{
  /** Adapts a `String` to [[Readable]] by exposing its UTF-8 bytes through a stream.
   *
   *  @param s the `String` exposed as bytes; must be non-null,
   *  otherwise `readBytesThrough` and `contentLength` throw a `NullPointerException`.
   */
  implicit class StringReadable(s: String) extends Readable{
    /** Runs `f` on a `ByteArrayInputStream` over the UTF-8 encoding of `s` and returns its result.
     *
     *  @tparam T the result type produced by `f`.
     *  @param f the callback receiving the fresh stream; must be non-null and must not use the stream after it returns.
     *  @return the value returned by `f`; exceptions from `f` propagate unchanged.
     */
    def readBytesThrough[T](f: InputStream => T): T = {
      f(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    }
    /** Returns `Some("text/plain; charset=utf-8")` for the UTF-8 encoding exposed by [[StringReadable]]. */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** Returns `Some` of the UTF-8 byte length of `s`, computed by [[Internal.encodedLength]] without encoding. */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Adapts an `Array[Byte]` to [[Readable]] by exposing it through a stream.
   *
   *  @param a the array exposed as bytes; must be non-null,
   *  otherwise `readBytesThrough` throws a `NullPointerException`, and later mutation of `a` is visible to already opened streams.
   */
  implicit class ByteArrayReadable(a: Array[Byte]) extends Readable{
    /** Runs `f` on a `ByteArrayInputStream` wrapping `a` and returns its result.
     *
     *  @tparam T the result type produced by `f`.
     *  @param f the callback receiving the fresh stream; must be non-null and must not use the stream after it returns.
     *  @return the value returned by `f`; exceptions from `f` propagate unchanged.
     */
    def readBytesThrough[T](f: InputStream => T): T = f(new ByteArrayInputStream(a))
    /** Returns `Some("application/octet-stream")`, as raw bytes carry no text type. */
    override def httpContentType = Some("application/octet-stream")
    /** Returns `Some(a.length)`, the exact number of bytes the stream exposes. */
    override def contentLength = Some(a.length)
  }

  /** Adapts a `java.nio.ByteBuffer` to [[Readable]] over its remaining bytes.
   *
   *  @param buffer the buffer whose `remaining` bytes are exposed; must be non-null,
   *  otherwise `readBytesThrough` and `contentLength` throw a `NullPointerException`.
   */
  implicit class ByteBufferReadable(buffer: java.nio.ByteBuffer) extends Readable{
    /** Runs `f` on an `InputStream` view over a duplicate of `buffer` and returns its result, leaving the original position unchanged.
     *
     *  @tparam T the result type produced by `f`.
     *  @param f the callback receiving the stream view, which returns `-1` once the duplicated `remaining` bytes are consumed; must be non-null and must not use the stream after it returns.
     *  @return the value returned by `f`; exceptions from `f` propagate unchanged.
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
    /** Returns `Some("application/octet-stream")`, as raw bytes carry no text type. */
    override def httpContentType = Some("application/octet-stream")
    /** Returns `Some(buffer.remaining())`, the number of bytes the stream view exposes at call time. */
    override def contentLength = Some(buffer.remaining())
  }

  /** Adapts an `InputStream` to [[Readable]] by passing it directly to callbacks.
   *
   *  @param i the underlying stream handed to every callback; must be non-null and open,
   *  otherwise `readBytesThrough` throws a `NullPointerException` or relays the `IOException` from the callback.
   */
  implicit class InputStreamReadable(i: InputStream) extends Readable{
    /** Runs `f` directly on the wrapped stream `i` and returns its result, without wrapping, buffering, or closing it.
     *
     *  @tparam T the result type produced by `f`.
     *  @param f the callback receiving `i` itself; must be non-null and must not assume `i` is freshly positioned or remains usable afterwards.
     *  @return the value returned by `f`; exceptions from `f` propagate unchanged.
     */
    def readBytesThrough[T](f: InputStream => T): T = f(i)
    /** Returns `Some("application/octet-stream")`, as the wrapped stream is assumed to carry raw bytes. */
    override def httpContentType: Option[String] = Some("application/octet-stream")
  }
}
