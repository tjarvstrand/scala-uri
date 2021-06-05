package io.lemonlabs.uri

import io.lemonlabs.uri.config.UriConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NormalizationTests extends AnyFlatSpec with Matchers {
  implicit val config: UriConfig = UriConfig.default

  "UserInfo" should "normalize correctly" in {
    UserInfo("pirate", "parrot").normalize() should equal(UserInfo("pirate", "parrot"))
    UserInfo("pirate", "parrot").normalize(true) should equal(UserInfo("pirate", "parrot"))
    UserInfo("pir%61te", "p%61rrot").normalize() should equal(UserInfo("pir%61te", "p%61rrot"))
    UserInfo("pir%61te", "p%61rrot").normalize(true) should equal(UserInfo("pirate", "parrot"))
    UserInfo("pir%2fte", "p%2frrot").normalize() should equal(UserInfo("pir%2Fte", "p%2Frrot"))
    UserInfo("pir%2fte", "p%2frrot").normalize(true) should equal(UserInfo("pir%2Fte", "p%2Frrot"))
  }

  "Host" should "normalize correctly" in {
    IpV4.parse("192.168.0.1").normalize should equal(IpV4.parse("192.168.0.1"))
    IpV6.parse("[2001:0db8:85a3:0000:0000:8a2e:0370:7334]") should equal(
      IpV6.parse("[2001:0db8:85a3:0000:0000:8a2e:0370:7334]")
    )
    DomainName("Foo.bar.com").normalize should equal(DomainName("foo.bar.com"))
  }

  "Authority" should "normalize correctly" in {
    Authority(Some(UserInfo("pir%2fte", "p%2frrot")), DomainName("Foo.bar.com"), None).normalize() should equal(
      Authority(Some(UserInfo("pir%2Fte", "p%2Frrot")), DomainName("foo.bar.com"), None)
    )
  }

  "Path" should "normalize correctly" in {
    AbsolutePath.fromParts("").normalize() should equal(AbsolutePath.fromParts(""))
    AbsolutePath.fromParts("A").normalize() should equal(AbsolutePath.fromParts("A"))
    AbsolutePath.fromParts("%2f").normalize() should equal(AbsolutePath.fromParts("%2F"))
    AbsolutePath.fromParts("%61").normalize() should equal(AbsolutePath.fromParts("%61"))
    AbsolutePath.fromParts("%61").normalize(true) should equal(AbsolutePath.fromParts("a"))
    AbsolutePath.fromParts(".").normalize() should equal(AbsolutePath.fromParts(""))
    AbsolutePath.fromParts("..").normalize() should equal(AbsolutePath.fromParts(""))
    AbsolutePath.fromParts("a", ".", "b").normalize() should equal(AbsolutePath.fromParts("a", "b"))
    AbsolutePath.fromParts("a", "..", "b").normalize() should equal(AbsolutePath.fromParts("b"))

  }

//  "AbsoluteUrl" should "normalize the url" in {
//    //println(AbsoluteUrl.parse("fTp://Foo.bar/Foo%2fb%61r?Foo=%2fb%61r#Foo%2fb%61r"))
//    val url = AbsoluteUrl(
//      scheme = "fTp",
//      authority = Authority(
//        userInfo = Some(UserInfo("pir%61te", "a%2f%61b")),
//        host = DomainName("Foo.bar"),
//        port = None
//      ),
//      path = AbsolutePath.apply(Vector("Foo%2fb%61r")),
//      query = QueryString.fromPairs("Foo" -> "%2fb%61r"),
//      fragment = Some("Foo%2fb%61r")
//    )
////    AbsoluteUrl
////      .parse("fTp://pir%2fte:a%2f%61b@Foo.bar/Foo%2fb%61r?Foo=%2fb%61r#Foo%2fb%61r")
//
//    url.normalize().toString() should equal(
//      "ftp://pir%2Fte:a%2Fab@foo.bar/Foo%2Fb%61r?Foo=%2Fb%61r#Foo/bar"
//    )
////    AbsoluteUrl.parse("ftp://Foo.bar/").normalize() should equal(AbsoluteUrl.parse("ftp://foo.bar/"))
////    AbsoluteUrl.parse("ftp://foo.bar/%2f").normalize() should equal(AbsoluteUrl.parse("ftp://foo.bar/%2F"))
////    AbsoluteUrl.parse("ftp://Foo.bar/baz=b%2fam").normalize() should equal(
////      AbsoluteUrl.parse("ftp://foo.bar/baz=b%2Fam")
////    )
////    AbsoluteUrl.parse("ftp://192.168.1.1/%2f").normalize() should equal(AbsoluteUrl.parse("ftp://192.168.1.1/%2F"))
////    AbsoluteUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2f").normalize() should equal(
////      AbsoluteUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2F")
////    )
//  }
//
//  it should "HTTP-normalize the URL" in {
//    AbsoluteUrl.parse("http://foo.bar/").normalizeHttp should equal(AbsoluteUrl.parse("http://foo.bar/"))
//    AbsoluteUrl.parse("http://Foo.bar/%61").normalizeHttp should equal(AbsoluteUrl.parse("http://foo.bar/a"))
//    AbsoluteUrl.parse("http://foo.bar:80/").normalizeHttp should equal(AbsoluteUrl.parse("http://foo.bar/"))
//  }
//
//  "RelativeUrl" should "normalize the url" in {
//    RelativeUrl.parse("Foob%61%2F?a=c%2Fd#").normalize() should equal(RelativeUrl.parse("Foo"))
//    RelativeUrl.parse("Foo/").normalize() should equal(RelativeUrl.parse("Foo/"))
//    RelativeUrl.parse("ftp://foo.bar/%2f").normalize() should equal(RelativeUrl.parse("ftp://foo.bar/%2F"))
//    RelativeUrl.parse("ftp://Foo.bar/baz=b%2fam").normalize() should equal(
//      RelativeUrl.parse("ftp://foo.bar/baz=b%2Fam")
//    )
//    RelativeUrl.parse("ftp://192.168.1.1/%2f").normalize() should equal(RelativeUrl.parse("ftp://192.168.1.1/%2F"))
//    RelativeUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2f").normalize() should equal(
//      RelativeUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2F")
//    )
//  }
//
  "DataUrl.normalize" should "normalize the url" in {
    DataUrl.parse("data:text/plain;charset=UTF-8;page=21,the%2Fdata:1234,5678").normalize() should equal(
      DataUrl.parse("data:text/plain;charset=UTF-8;page=21,the/data:1234,5678")
    )
  }
}
