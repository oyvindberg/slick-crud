package com.olvind.crud

import scala.concurrent.duration._

case class TimedT[+T](started: Instant, duration: FiniteDuration, value: T)

case class Instant(value: Long) extends AnyVal

object Instant{
  def now = Instant(System.currentTimeMillis)
}

class Clock private (t0: Instant) {
  def td: FiniteDuration =
    (System.currentTimeMillis - t0.value).millis
  def timed[T](t: T) =
    TimedT(t0, td, t)
  def withTd[T](f: FiniteDuration ⇒ T): T =
    f(td)
}

object Clock {
  def apply[T](t: Clock ⇒ T): T = {
    t(new Clock(Instant.now))
  }
}
