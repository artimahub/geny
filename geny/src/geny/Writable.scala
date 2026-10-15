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
  /** Writes the complete payload of this [[Writable]] to `out`, in order, and
   *  returns once every byte has been handed to the stream.
   *
   *  The stream belongs to the caller: implementations must leave it open and
   *  unflushed and must not close it, so flushing and closing stay the caller's
   *  job.
   *
   *  @param out the stream that receives the bytes. It is written to directly,
   *             so a `null` is not rejected up front — it surfaces as a
   *             `NullPointerException` from the first write that actually
   *             carries bytes, which means an empty payload may never notice.
   *             A failure in `out` (a full disk, a closed socket) propagates as
   *             an `IOException`, leaving the payload partially written.
   */
  def writeBytesTo(out: OutputStream): Unit
  /** The MIME type this payload should be served with over HTTP, when this
   *  library knows one.
   *
   *  `None`, the default, means "unknown", not "no type": a caller that needs a
   *  value has to substitute a fallback of its own or refuse the request.
   *  Nothing validates the string, so an implementation overriding this is
   *  responsible for emitting a well-formed media type.
   */
  def httpContentType: Option[String] = None
  /** How many bytes [[writeBytesTo]] will write, if that is known before the
   *  bytes are produced.
   *
   *  `None`, the default, means the length is unknown and the payload must be
   *  streamed rather than sent with a `Content-Length`. `Some(n)` is only a
   *  promise: nothing compares it against what is actually written, so an
   *  implementation whose payload can change must recompute rather than cache.
   */
  def contentLength: Option[Long] = None
}
/** The companion of [[Writable]], holding the implicit conversions that build
 *  one from a `String`, an `Array[Byte]` or a `java.nio.ByteBuffer`. It extends
 *  [[LowPriWritable]], which puts the `readableWritable` conversion in implicit
 *  scope alongside them.
 */
object Writable extends LowPriWritable {
  /** Implicitly converts a [[String]] into a [[Writable]] that emits its UTF-8
   *  encoding, so `"hello".writeBytesTo(out)` compiles.
   *
   *  The string is kept as-is and encoded only when the bytes are written.
   *
   *  @param s the string to encode. It is read afresh on every write, so `null`
   *           fails with `NullPointerException` rather than producing an empty
   *           payload; `""` is valid and writes zero bytes.
   */
  implicit class StringWritable(s: String) extends Writable{
    /** Encodes the string as UTF-8 and writes it to `out` in chunks of 8192
     *  characters, one `OutputStream.write(byte[])` call per chunk.
     *
     *  Chunking happens on the characters *before* encoding, so a surrogate
     *  pair that straddles an 8192-char boundary is emitted as two `?`
     *  replacement bytes instead of one 4-byte sequence. Apart from that the
     *  bytes written are what `s.getBytes(UTF_8)` would produce; no byte-order
     *  mark is emitted.
     *
     *  @param out the stream that receives the UTF-8 bytes. It is neither
     *             flushed nor closed here. A `null` fails with
     *             `NullPointerException` on the first chunk, which for `""`
     *             never comes, since an empty string produces no chunks.
     */
    def writeBytesTo(out: OutputStream): Unit = {

      s.grouped(8192).foreach(ss => out.write(ss.getBytes(StandardCharsets.UTF_8)))
    }
    /** `Some("text/plain; charset=utf-8")`, matching the encoding
     *  [[writeBytesTo]] performs, so the declared type can never contradict the
     *  bytes. Constant, and therefore identical on every call.
     */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** The UTF-8 length of the string as computed by [[Internal.encodedLength]],
     *  which measures the string without encoding it.
     *
     *  Recomputed on each call. It agrees with what [[writeBytesTo]] emits,
     *  except that a lone surrogate is rejected here with
     *  `IllegalArgumentException` while the encoder would have substituted `?`.
     */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Implicitly converts an `Array[Byte]` into a [[Writable]] that emits those
   *  bytes verbatim, so `bytes.writeBytesTo(out)` compiles.
   *
   *  No copy is made: the array is referenced, so mutating it afterwards changes
   *  what a subsequent write produces.
   *
   *  @param a the exact bytes to write. Every element is passed on as a raw
   *           byte, never decoded into a character; `null` fails with
   *           `NullPointerException`, and an empty array writes nothing at all.
   */
  implicit class ByteArrayWritable(a: Array[Byte]) extends Writable{
    /** Writes the whole array to `out` in a single
     *  `OutputStream.write(byte[])` call, handing the payload over in one go
     *  rather than streaming it in chunks.
     *
     *  The array is only read, never copied or modified, and `out` is left open
     *  and unflushed.
     *
     *  @param out the stream that receives the array. A `null` fails with
     *             `NullPointerException`; an empty array performs no write and
     *             so never notices a `null`.
     */
    def writeBytesTo(out: OutputStream): Unit = out.write(a)
    /** `Some("application/octet-stream")`: raw bytes carrying no encoding of
     *  their own. Constant, and independent of the array's contents.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some(a.length)`, re-read on each call rather than cached.
     *
     *  An array's length is fixed for its lifetime, so this always agrees with
     *  the number of bytes [[writeBytesTo]] hands to `out`. Mutating the array's
     *  contents in between changes which bytes are written, not how many.
     */
    override def contentLength = Some(a.length)
  }

  /** Implicitly converts a `java.nio.ByteBuffer` into a [[Writable]] that emits
   *  the bytes between the buffer's position and its limit, so
   *  `buffer.writeBytesTo(out)` compiles.
   *
   *  The buffer is not consumed: writing duplicates it, which leaves the
   *  original's position, limit and mark untouched and lets the same buffer be
   *  written repeatedly. The bytes are read live, so the buffer must not be
   *  mutated by anyone else while a write is in progress.
   *
   *  @param buffer the buffer whose remaining bytes form the payload, that is
   *                `buffer.remaining()` bytes starting at the current position.
   *                A `null` fails with `NullPointerException`. The byte order is
   *                carried over to the duplicate, though a bulk copy is
   *                byte-order agnostic; `position`, `limit` and `mark` are left
   *                as they were.
   */
  implicit class ByteBufferWritable(buffer: java.nio.ByteBuffer) extends Writable {
    /** Copies the buffer's remaining bytes into `out` through a reusable 8 KiB
     *  staging array, issuing one `OutputStream.write(byte[], int, int)` call per
     *  chunk.
     *
     *  The buffer is duplicated first, so the caller's position and limit are
     *  unchanged and a second call writes the same bytes again. `out` is left
     *  open and unflushed, and the staging array is reused across chunks, which
     *  is safe only because each chunk is written before the next one is read.
     *
     *  @param out the stream that receives the bytes. A `null` fails with
     *             `NullPointerException` on the first chunk; a buffer with
     *             nothing remaining writes nothing and so never touches `out`.
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
    /** `Some("application/octet-stream")`, as for the other raw-byte
     *  [[Writable]]s. Constant, and correct for any buffer since raw bytes are
     *  moved without decoding.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some(buffer.remaining())`, evaluated against the live buffer on every
     *  call instead of being cached at construction.
     *
     *  Because [[writeBytesTo]] duplicates the buffer, the value does not fall
     *  to zero after a write; it only goes stale if the caller moves the
     *  position or limit in between the two calls.
     */
    override def contentLength = Some(buffer.remaining())
  }

}

/** The low-priority implicit conversions into [[Writable]].
 *
 *  Mixed into the `Writable` companion object so that any value of a type `T`
 *  for which a `T => Readable` conversion is in scope can be used where a
 *  [[Writable]] is expected. It sits at low priority deliberately: the
 *  specialised conversions defined directly in the companions of `Writable` and
 *  `Readable` win for the types they cover, so this generic rule only fires for
 *  types that have no conversion of their own.
 */
trait LowPriWritable{
  /** Lifts any value that can be converted into a [[Readable]] into a
   *  [[Writable]], returning the very same instance.
   *
   *  Nothing is copied or buffered here: the result *is* the `Readable`, so
   *  writing it runs that object's `writeBytesTo`, which for a stream-backed
   *  source consumes the underlying stream for good.
   *
   *  @tparam T the type being converted; entirely unconstrained, so the
   *            conversion is selected purely by the availability of `f`.
   *  @param t the value to wrap. It is not inspected, so `null` is handed to `f`
   *           and whatever that does with it — most likely a
   *           `NullPointerException` — is what the caller sees.
   *  @param f the implicit conversion from `T` to a `T`-specific `Readable`,
   *           typically one of the implicit classes in `Readable`'s companion.
   *           When no such conversion is in scope, the call fails to compile
   *           rather than at run time.
   *  @return `f(t)` itself, a `Readable` widened to `Writable`. The identity is
   *          preserved: this is not a wrapper that could fall out of sync with
   *          the original.
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
  /** Hands a stream over this [[Readable]]'s bytes to `f` and returns whatever
   *  `f` returns, so the payload can be consumed with blocking `read` calls
   *  instead of being pushed out by [[writeBytesTo]].
   *
   *  The stream only has to exist for the duration of the call: an
   *  implementation may close it and release whatever is behind it as soon as
   *  `f` returns, so callers must not retain or use it afterwards.
   *
   *  @tparam T the result type of the callback. It is unrelated to the payload
   *            and is returned unchanged.
   *  @param f the function to run over the stream. It is invoked exactly once,
   *           synchronously, on the calling thread, and receives a stream
   *           positioned at the first byte of the payload. An exception thrown
   *           by `f` propagates to the caller of this method.
   *  @return the value `f` produced, passed straight through. Failure is
   *          reported by throwing — from `f`, or later from a read on the
   *          stream — never as a sentinel value, so nothing needs unwrapping.
   */
  def readBytesThrough[T](f: InputStream => T): T
  /** Copies every byte of the payload into `out` by running
   *  [[Internal.transfer]] over a stream obtained from [[readBytesThrough]].
   *
   *  The copy is streamed through a transfer buffer that starts near the
   *  source's `available` size and grows up to a ceiling, so neither stream has
   *  to hold the whole payload. That source stream is closed before this method
   *  returns; `out` is left open and unflushed.
   *
   *  @param out the stream that receives the bytes, in order. It is neither
   *             flushed nor closed here. A `null` fails with
   *             `NullPointerException` on the first write, so an empty payload
   *             never notices.
   */
  def writeBytesTo(out: OutputStream): Unit = readBytesThrough(Internal.transfer(_, out))
}
/** The companion of [[Readable]], holding the implicit conversions that build
 *  one from a `String`, an `Array[Byte]`, a `java.nio.ByteBuffer` or an
 *  `InputStream`.
 */
object Readable{
  /** Implicitly converts a [[String]] into a [[Readable]] over its UTF-8
   *  encoding, so `"hello".readBytesThrough(f)` compiles.
   *
   *  The string is encoded per call rather than up front, which makes the result
   *  reusable: every call starts from the beginning.
   *
   *  @param s the string to encode. `null` fails with `NullPointerException`
   *           instead of yielding an empty stream; `""` is valid and produces a
   *           stream that is immediately at EOF.
   */
  implicit class StringReadable(s: String) extends Readable{
    /** Encodes the string as UTF-8 and hands `f` a `ByteArrayInputStream` over
     *  the result.
     *
     *  A fresh stream is built on every call, so the same [[StringReadable]] can
     *  be read repeatedly and always sees the whole string. The stream holds no
     *  external resource, so `f` may ignore it or leave it unclosed without
     *  leaking anything.
     *
     *  @tparam T the result type of `f`, returned unchanged.
     *  @param f the function to run, called once and synchronously with a stream
     *           positioned at the first UTF-8 byte. An exception it throws
     *           propagates to the caller.
     *  @return the value `f` returned. Running out of bytes is reported by
     *          `read` returning -1, not by this method, which always returns
     *          `f`'s own result.
     */
    def readBytesThrough[T](f: InputStream => T): T = {
      f(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    }
    /** `Some("text/plain; charset=utf-8")`, matching the encoding
     *  [[readBytesThrough]] performs. Constant, so it cannot disagree with the
     *  bytes that are produced.
     */
    override def httpContentType = Some("text/plain; charset=utf-8")
    /** The UTF-8 length of the string from [[Internal.encodedLength]], which
     *  measures it without encoding, re-read on each call.
     *
     *  It matches the length of what [[readBytesThrough]] hands out, except
     *  that an unpaired surrogate raises `IllegalArgumentException` here while
     *  the encoder would have substituted a single `?` byte.
     */
    override def contentLength = Some(Internal.encodedLength(s))
  }

  /** Implicitly converts an `Array[Byte]` into a [[Readable]] over exactly those
   *  bytes, so `bytes.readBytesThrough(f)` compiles.
   *
   *  The array is not copied, so the payload reflects any later mutation of it.
   *
   *  @param a the bytes to read. They are exposed raw and never decoded;
   *           `null` fails with `NullPointerException`, and an empty array
   *           produces a stream that is immediately at EOF.
   */
  implicit class ByteArrayReadable(a: Array[Byte]) extends Readable{
    /** Hands `f` a `ByteArrayInputStream` positioned at the start of the array.
     *
     *  Reading from that stream does not modify the array, and a new one is
     *  built per call, so this [[Readable]] is reusable and every call starts at
     *  index 0.
     *
     *  @tparam T the result type of `f`, returned unchanged.
     *  @param f the function to run over the stream, called once and
     *           synchronously. It may read as much or as little as it likes; an
     *           exception it throws propagates to the caller.
     *  @return the value `f` returned, passed through untouched — there is no
     *          error channel besides exceptions.
     */
    def readBytesThrough[T](f: InputStream => T): T = f(new ByteArrayInputStream(a))
    /** `Some("application/octet-stream")`, since an array of bytes carries no
     *  encoding of its own. Constant.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some(a.length)`, re-read on each call so that it tracks mutation of the
     *  array, which is also exactly what a later read will see.
     */
    override def contentLength = Some(a.length)
  }

  /** Implicitly converts a `java.nio.ByteBuffer` into a [[Readable]] over the
   *  bytes between its position and its limit, so `buffer.readBytesThrough(f)`
   *  compiles.
   *
   *  Each call duplicates the buffer, so the original's position, limit and mark
   *  are untouched and the same bytes can be read again — though not safely
   *  while another thread mutates the buffer underneath.
   *
   *  @param buffer the buffer whose remaining bytes form the payload, that is
   *                `buffer.remaining()` bytes starting at the current position.
   *                A `null` fails with `NullPointerException`. Byte order is
   *                copied onto the duplicate but does not affect the result,
   *                since the bytes are moved verbatim.
   */
  implicit class ByteBufferReadable(buffer: java.nio.ByteBuffer) extends Readable{
    /** Wraps the buffer's remaining bytes in a streaming `InputStream` view and
     *  hands it to `f`.
     *
     *  The view reads straight out of a fresh duplicate, so it delivers the same
     *  bytes on every call. Only `read()` and `read(byte[], int, int)` are
     *  implemented: `close` is a no-op because there is nothing behind the
     *  buffer to release, and `available` reports 0, since no count of the
     *  remaining bytes is maintained. A short read is possible and must be
     *  handled by the caller.
     *
     *  @tparam T the result type of `f`, returned unchanged.
     *  @param f the function to run over the stream, called once and
     *           synchronously. If it reads the stream and then throws, the
     *           exception propagates and the bytes it consumed are lost for good,
     *           because there is no way to rewind the duplicate.
     *  @return the value `f` returned. End of data is signalled by `read`
     *          returning -1, not by this method, which always returns `f`'s own
     *          result.
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
    /** `Some("application/octet-stream")`, as for the other raw-byte
     *  [[Readable]]s. Constant, and correct for any buffer because the bytes are
     *  moved without decoding.
     */
    override def httpContentType = Some("application/octet-stream")
    /** `Some(buffer.remaining())`, evaluated against the live buffer on each
     *  call rather than cached at construction.
     *
     *  Reading through [[readBytesThrough]] consumes only a duplicate, so this
     *  does not drop to zero afterwards; it goes stale only if the caller moves
     *  the position or limit in between.
     */
    override def contentLength = Some(buffer.remaining())
  }

  /** Implicitly converts an `InputStream` into a [[Readable]] that hands that
   *  very stream to a callback, so `is.readBytesThrough(f)` compiles.
   *
   *  Unlike the other [[Readable]]s this one is single-use: the stream is passed
   *  on as-is, never rewound or copied, so its contents can be read only once.
   *
   *  @param i the stream to expose. It is not buffered, wrapped or decoded, and
   *           a `null` is passed straight through, so whether that misbehaves at
   *           all depends on `f` — in practice a `NullPointerException` on its
   *           first use.
   */
  implicit class InputStreamReadable(i: InputStream) extends Readable{
    /** Passes the wrapped stream directly to `f` and returns its result,
     *  without reading, buffering or closing anything itself.
     *
     *  Closing the stream, and whatever resource sits behind it, is left to `f`
     *  or to the caller of this method: nothing here closes it. Note that
     *  [[writeBytesTo]] does close it, by routing through
     *  [[Internal.transfer]]. Since the same instance is handed out every time,
     *  a second call continues where the first stopped and typically sees EOF
     *  straight away.
     *
     *  @tparam T the result type of `f`, returned unchanged.
     *  @param f the function to run, called once and synchronously on the
     *           calling thread. It owns the stream for as long as it needs it,
     *           and should close it if that matters; an exception it throws
     *           propagates to the caller and leaves the stream at whatever
     *           position it had reached.
     *  @return the value `f` returned, passed through untouched. Failure is
     *          reported only by exceptions — a stream that is already exhausted
     *          simply yields -1 from `read`.
     */
    def readBytesThrough[T](f: InputStream => T): T = f(i)
    /** `Some("application/octet-stream")`, the conservative default for a stream
     *  whose bytes carry no decoding of their own.
     *
     *  [[contentLength]] is deliberately left at its inherited `None`: a stream
     *  generally does not know its size, and the first read consumes it, so
     *  reporting a length would be a guess.
     */
    override def httpContentType: Option[String] = Some("application/octet-stream")
  }
}
