#!/usr/bin/env sh

sbt +test +clean +coverageOff +publishSigned && \
  sbt ++0.24.0 test clean coverageOff publishSigned
