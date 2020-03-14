package io.lemonlabs.uri.decoding

import io.lemonlabs.uri.UriException

case class UriDecodeException(message: String) extends UriException(message)
