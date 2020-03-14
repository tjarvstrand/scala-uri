package io.lemonlabs.uri.parsing

import io.lemonlabs.uri.UriException

case class UriParsingException(msg: String) extends UriException(msg)
