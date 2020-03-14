package io.lemonlabs.uri

import cats.syntax.functor._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.parser._
import io.lemonlabs.uri.config.UriConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source
import scala.util.Success

class WebPlatformTests extends AnyWordSpec with Matchers {

  sealed trait ParseTestCase

  case class TestDescription(title: String) extends ParseTestCase

  case class ParseFailureTestCase(
      input: String,
      failure: Boolean
  ) extends ParseTestCase

  case class ParseSuccessTestCase(
      input: String,
      href: String,
      origin: Option[String],
      protocol: String,
      username: String,
      password: String,
      host: String,
      hostname: String,
      port: String,
      pathname: String,
      search: String,
      hash: String
  ) extends ParseTestCase

  val parseFailureTestCaseDecoder: Decoder[ParseTestCase] = deriveDecoder[ParseFailureTestCase].widen
  val parseSuccessTestCaseDecoder: Decoder[ParseTestCase] = deriveDecoder[ParseSuccessTestCase].widen
  val parseTestTitleDecoder: Decoder[ParseTestCase] =
    Decoder.decodeString.map(TestDescription.apply).widen

  implicit val parseTestCase: Decoder[ParseTestCase] =
    parseFailureTestCaseDecoder or parseSuccessTestCaseDecoder or parseTestTitleDecoder

  val testCases: Seq[ParseTestCase] = {
    // TODO: Source.fromResource is not supported in Scala.js. Find alternative
    val json = Source.fromResource("web-platform-tests/urltestdata.json").getLines.mkString
    parse(json)
      .fold(e => fail(s"Failed to parse JSON: $e"), identity)
      .as[Seq[ParseTestCase]]
      .fold(e => fail(s"Failed to decode JSON: $e"), identity)
  }

  "Uri.parse" should {
    testCases.zipWithIndex.foreach {
      case (
          t @ ParseSuccessTestCase(
            input,
            _,
            origin,
            protocol,
            username,
            password,
            _,
            hostname,
            port,
            pathname,
            search,
            hash
          ),
          i
          ) =>
        s"$i. successfully parse $input" in withClue(t) {
          // scala-uri does not ignore whitespace (e.g. \n, \t)
          // Should it? Make it a config option?
          val inputNoWhitespace =
            input.replaceAll("[\t\n]", "")

          val uri = Uri.parseTry(inputNoWhitespace)

          uri match {
            case Success(_: RelativeUrl) =>
              cancel(
                "Ignoring. scala-uri doesn't yet have a the concept of Origin from the whatwg spec, " +
                  "so cannot run RelativeUrl tests"
              )

            case _ if origin.isDefined =>
              cancel(
                "Ignoring. scala-uri doesn't yet have a the concept of Origin from the whatwg spec, " +
                  "so cannot run tests with an origin"
              )

            case Success(url: Url) =>
              url.schemeOption.fold("")(_ + ":") should equal(protocol)
              url.user.getOrElse("") should equal(username)
              url.password.getOrElse("") should equal(password)
              url.hostOption.fold("")(_.toString()) should equal(hostname)
              url.port.fold("")(_.toString) should equal(port)
              url.path.toStringRaw should equal(pathname)
              url.query.toString() should equal(search.dropWhile(_ == '?'))
              url.fragmentToString(UriConfig.default) should equal(hash)

            case Success(urn: Urn) =>
              urn.schemeOption.fold("")(_ + ":") should equal(protocol)
              urn.path.toString() should equal(pathname)

            case other =>
              fail("Failed to parse URL: " + other)
          }
        }

      case (t @ ParseFailureTestCase(input, true), i) =>
        s"$i. fail to parse $input" in withClue(t) {
          val url = Url.parseTry(input)
          url.isSuccess should equal(false)
        }

      case (TestDescription(t), _) =>
        // TODO: merge these into the other tests
        println(t)

      case other =>
        fail(s"Invalid test case: $other")
    }
  }
}
