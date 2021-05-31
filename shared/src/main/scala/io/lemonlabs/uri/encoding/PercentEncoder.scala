package io.lemonlabs.uri.encoding

import PercentEncoder._
import io.lemonlabs.uri.decoding.PercentDecoder

import scala.annotation.tailrec

case class PercentEncoder(charsToEncode: Set[Char] = DEFAULT_CHARS_TO_ENCODE) extends UriEncoder {
  def shouldEncode(ch: Char) = {
    !PercentEncoder.ascii(ch) || charsToEncode.contains(ch)
  }
  override def encodeChar(ch: Char): String = PercentEncoder.encodeChar(ch)

  @deprecated("Use PercentEncoder.ascii instead", "3.3.0")
  def ascii(ch: Char): Boolean = PercentEncoder.ascii(ch)
  @deprecated("Use PercentEncoder.toHex instead", "3.3.0")
  def toHex(ch: Char): String = PercentEncoder.toHex(ch)

  def --(chars: Char*) = new PercentEncoder(charsToEncode.diff(chars.toSet))
  def ++(chars: Char*) = new PercentEncoder(charsToEncode ++ chars)
}

object PercentEncoder {
  val USER_INFO_CHARS_TO_ENCODE = Set(
    ' ', '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|', '?', '@', ':', '/'
  )

  val PATH_CHARS_TO_ENCODE = Set(
    ' ', '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|', '?', '/'
  )

  val QUERY_CHARS_TO_ENCODE = Set(
    '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|', '&', '\\', '+', '='
  )

  val FRAGMENT_CHARS_TO_ENCODE = Set(
    ' ', '%', '<', '>', '[', ']', '#', '{', '}', '^', '`', '|'
  )

  val GEN_DELIMS = Set(':', '/', '?', '#', '[', ']', '@')
  val SUB_DELIMS = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')
  val RESERVED = GEN_DELIMS ++ SUB_DELIMS
  lazy val UNRESERVED: Set[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890-._~".toSet

  val EXCLUDED = Set('"') // RFC 2396 section 2.4.3

  /** Probably more than you need to percent encode. Wherever possible try to use a tighter Set of characters
    * to encode depending on your use case
    */
  val DEFAULT_CHARS_TO_ENCODE = RESERVED ++ PATH_CHARS_TO_ENCODE ++ QUERY_CHARS_TO_ENCODE ++ EXCLUDED

  def encodeChar(ch: Char): String = "%" + toHex(ch)
  def normalize(s: String, decodeUnreserved: Boolean): String = normalize(s, decodeUnreserved, Seq.empty)

  @tailrec
  private def normalize(s: String, decodeUnreserved: Boolean, parts: Seq[String]): String = {
    val index = s.indexOf('%')
    if (index == -1 || s.length < index + 3) {
      parts.reverse.mkString ++ s
    } else {
      val (prefix, suffix) = s.splitAt(index)
      val (encoded, rest) = suffix.splitAt(3)
      val decoded = PercentDecoder.decode(encoded)
      val result = if (decodeUnreserved && UNRESERVED.contains(decoded.charAt(0))) decoded else encoded.toUpperCase
      normalize(rest, decodeUnreserved, (prefix ++ result) +: parts)
    }
  }

  def toHex(ch: Char): String = "%04x".format(ch.toInt).substring(2).toUpperCase

  /** Determines if this character is in the ASCII range (excluding control characters)
    */
  def ascii(ch: Char): Boolean = ch > 31 && ch < 127
}
