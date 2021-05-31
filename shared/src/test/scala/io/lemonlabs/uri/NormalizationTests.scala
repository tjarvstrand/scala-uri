package io.lemonlabs.uri

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NormalizationTests extends AnyFlatSpec with Matchers {
  "AbsoluteUrl" should "normalize the url" in {
    AbsoluteUrl.parse("fTp://").normalize() should equal(AbsoluteUrl.parse("ftp://"))
    AbsoluteUrl.parse("ftp://Foo.bar").normalize() should equal(AbsoluteUrl.parse("ftp://foo.bar"))
    AbsoluteUrl.parse("ftp://Foo.bar/").normalize() should equal(AbsoluteUrl.parse("ftp://foo.bar/"))
    AbsoluteUrl.parse("ftp://foo.bar/%2f").normalize() should equal(AbsoluteUrl.parse("ftp://foo.bar/%2F"))
    AbsoluteUrl.parse("ftp://Foo.bar/baz=b%2fam").normalize() should equal(
      AbsoluteUrl.parse("ftp://foo.bar/baz=b%2Fam")
    )
    AbsoluteUrl.parse("ftp://192.168.1.1/%2f").normalize() should equal(AbsoluteUrl.parse("ftp://192.168.1.1/%2F"))
    AbsoluteUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2f").normalize() should equal(
      AbsoluteUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2F")
    )
  }

  it should "HTTP-normalize the URL" in {
    AbsoluteUrl.parse("http://foo.bar/").normalizeHttp should equal(AbsoluteUrl.parse("http://foo.bar/"))
    AbsoluteUrl.parse("http://Foo.bar/%61").normalizeHttp should equal(AbsoluteUrl.parse("http://foo.bar/a"))
    AbsoluteUrl.parse("http://foo.bar:80/").normalizeHttp should equal(AbsoluteUrl.parse("http://foo.bar/"))
  }

  "RelativeUrl" should "normalize the url" in {
    RelativeUrl.parse("Foob%61%2F?a=c%2Fd#").normalize() should equal(RelativeUrl.parse("Foo"))
    RelativeUrl.parse("Foo/").normalize() should equal(RelativeUrl.parse("Foo/"))
    RelativeUrl.parse("ftp://foo.bar/%2f").normalize() should equal(RelativeUrl.parse("ftp://foo.bar/%2F"))
    RelativeUrl.parse("ftp://Foo.bar/baz=b%2fam").normalize() should equal(
      RelativeUrl.parse("ftp://foo.bar/baz=b%2Fam")
    )
    RelativeUrl.parse("ftp://192.168.1.1/%2f").normalize() should equal(RelativeUrl.parse("ftp://192.168.1.1/%2F"))
    RelativeUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2f").normalize() should equal(
      RelativeUrl.parse("ftp://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/%2F")
    )
  }

  "DataUrl.normalize" should "normalize the url" in {
    DataUrl.parse("data:text/plain;charset=UTF-8;page=21,the%2Fdata:1234,5678").normalize() should equal(
      DataUrl.parse("data:text/plain;charset=UTF-8;page=21,the/data:1234,5678")
    )
  }
}
