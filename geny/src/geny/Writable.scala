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
  /** Writes the bytes of this value to the given OutputStream.
   *
   *  @param out the output stream to write the bytes to
   */
  def writeBytesTo(out: OutputStream): Unit
  /** The MIME content type of the bytes, if known, for use as an HTTP Content-Type header. */
  def httpContentType: Option[String] = None
  /** The number of bytes this writes, if known in advance. */
  def contentLength: Option[Long] = None
}
object Writable extends LowPriWritable {
  /** Implicit conversion from a `String` to a [[Writable]] that writes its UTF-8 bytes.
   *
   *  @param s the string to write
   */
  implicit class StringWritable(s: String) extends Writable{
    /** Writes the string to `out` as UTF-8 encoded bytes, in chunks of 8192 characters.
     *
     *  @param out the output stream to write the bytes to
     */
    def writeBytesTo(out: OutputStream): Unit = {

      s.grouped(8192).foreach(ss => out.write(ss.getBytes(StandardCharsets.UTF_8)))
    }
    /** The content type is `text/plain; charset=utf-8`. */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** The length of the string encoded as UTF-8, in bytes. */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Implicit conversion from a byte array to a [[Writable]] that writes its contents.
   *
   *  @param a the bytes to write
   */
  implicit class ByteArrayWritable(a: Array[Byte]) extends Writable{
    /** Writes the whole byte array to `out`.
     *
     *  @param out the output stream to write the bytes to
     */
    def writeBytesTo(out: OutputStream): Unit = out.write(a)
    /** The content type is `application/octet-stream`. */
    override def httpContentType = Some("application/octet-stream")
    /** The length of the byte array. */
    override def contentLength = Some(a.length)
  }

  /** Implicit conversion from a `ByteBuffer` to a [[Writable]] that writes its remaining bytes.
   *
   *  @param buffer the buffer whose remaining bytes are written
   */
  implicit class ByteBufferWritable(buffer: java.nio.ByteBuffer) extends Writable {
    /** Writes the remaining bytes of the buffer to `out`, in chunks of 8192 bytes.
     *
     *  @param out the output stream to write the bytes to
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
    /** The content type is `application/octet-stream`. */
    override def httpContentType = Some("application/octet-stream")
    /** The number of remaining bytes in the buffer. */
    override def contentLength = Some(buffer.remaining())
  }

}

/** Low-priority implicit conversions to [[Writable]], used when no conversion in the [[Writable]] companion applies. */
trait LowPriWritable{
  /** Converts any value that has an implicit [[Readable]] conversion into a [[Writable]].
   *
   *  @tparam T the type of value to convert
   *  @param t the value to convert
   *  @param f the implicit conversion from `T` to [[Readable]]
   *  @return a [[Writable]] that writes the bytes of the [[Readable]] obtained from `t`
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
  /** Calls `f` with an InputStream over the bytes of this value and returns its result.
   *  The stream is only valid during the call.
   *
   *  @tparam T the result type of `f`
   *  @param f the function that consumes the InputStream
   *  @return the result of `f`
   */
  def readBytesThrough[T](f: InputStream => T): T
  /** Writes the bytes of this value to `out` by reading them through [[readBytesThrough]].
   *
   *  @param out the output stream to write the bytes to
   */
  def writeBytesTo(out: OutputStream): Unit = readBytesThrough(Internal.transfer(_, out))
}
object Readable{
  /** Implicit conversion from a `String` to a [[Readable]] over its UTF-8 bytes.
   *
   *  @param s the string to read
   */
  implicit class StringReadable(s: String) extends Readable{
    /** Calls `f` with an InputStream over the UTF-8 bytes of the string and returns its result.
     *
     *  @tparam T the result type of `f`
     *  @param f the function that consumes the InputStream
     *  @return the result of `f`
     */
    def readBytesThrough[T](f: InputStream => T): T = {
      f(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    }
    /** The content type is `text/plain; charset=utf-8`. */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** The length of the string encoded as UTF-8, in bytes. */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Implicit conversion from a byte array to a [[Readable]] over its contents.
   *
   *  @param a the bytes to read
   */
  implicit class ByteArrayReadable(a: Array[Byte]) extends Readable{
    /** Calls `f` with an InputStream over the byte array and returns its result.
     *
     *  @tparam T the result type of `f`
     *  @param f the function that consumes the InputStream
     *  @return the result of `f`
     */
    def readBytesThrough[T](f: InputStream => T): T = f(new ByteArrayInputStream(a))
    /** The content type is `application/octet-stream`. */
    override def httpContentType = Some("application/octet-stream")
    /** The length of the byte array. */
    override def contentLength = Some(a.length)
  }

  /** Implicit conversion from a `ByteBuffer` to a [[Readable]] over its remaining bytes.
   *
   *  @param buffer the buffer whose remaining bytes are read
   */
  implicit class ByteBufferReadable(buffer: java.nio.ByteBuffer) extends Readable{
    /** Calls `f` with an InputStream over the remaining bytes of the buffer and returns its result.
     *
     *  @tparam T the result type of `f`
     *  @param f the function that consumes the InputStream
     *  @return the result of `f`
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
    /** The content type is `application/octet-stream`. */
    override def httpContentType = Some("application/octet-stream")
    /** The number of remaining bytes in the buffer. */
    override def contentLength = Some(buffer.remaining())
  }

  /** Implicit conversion from an `InputStream` to a [[Readable]] that wraps it.
   *
   *  @param i the input stream to read from
   */
  implicit class InputStreamReadable(i: InputStream) extends Readable{
    /** Passes the wrapped InputStream to `f` and returns its result.
     *
     *  @tparam T the result type of `f`
     *  @param f the function that consumes the InputStream
     *  @return the result of `f`
     */
    def readBytesThrough[T](f: InputStream => T): T = f(i)
    /** The content type is `application/octet-stream`. */
    override def httpContentType: Option[String] = Some("application/octet-stream")
  }
}
