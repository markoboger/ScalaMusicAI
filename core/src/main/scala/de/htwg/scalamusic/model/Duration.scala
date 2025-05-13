package de.htwg.scalamusic.model

import de.htwg.scalamusic.Duration

sealed trait DurationValue {
  val value: Duration
  def *(factor: Double): DurationValue
  def dotted: DurationValue
}

object DurationValue {
  // Standard note durations as fractions of a whole note
  case object Whole extends DurationValue {
    val value: Duration = 1.0
    def *(factor: Double): DurationValue = fromDouble(value * factor)
    def dotted: DurationValue = fromDouble(value * 1.5)
  }
  
  case object Half extends DurationValue {
    val value: Duration = 0.5
    def *(factor: Double): DurationValue = fromDouble(value * factor)
    def dotted: DurationValue = fromDouble(value * 1.5)
  }
  
  case object Quarter extends DurationValue {
    val value: Duration = 0.25
    def *(factor: Double): DurationValue = fromDouble(value * factor)
    def dotted: DurationValue = fromDouble(value * 1.5)
  }
  
  case object Eighth extends DurationValue {
    val value: Duration = 0.125
    def *(factor: Double): DurationValue = fromDouble(value * factor)
    def dotted: DurationValue = fromDouble(value * 1.5)
  }
  
  case object Sixteenth extends DurationValue {
    val value: Duration = 0.0625
    def *(factor: Double): DurationValue = fromDouble(value * factor)
    def dotted: DurationValue = fromDouble(value * 1.5)
  }
  
  case class CustomDuration(override val value: Duration) extends DurationValue {
    def *(factor: Double): DurationValue = CustomDuration(value * factor)
    def dotted: DurationValue = CustomDuration(value * 1.5)
  }
  
  def fromDouble(d: Double): DurationValue = d match {
    case 1.0 => Whole
    case 0.5 => Half
    case 0.25 => Quarter
    case 0.125 => Eighth
    case 0.0625 => Sixteenth
    case _ => CustomDuration(d)
  }
}

// For tuplets and other irregular durations
case class DurationMultiplier(factor: Double) extends AnyVal

object DurationMultiplier {
  val Triplet = DurationMultiplier(2.0/3)  // Three notes in the time of two
  val Quintuplet = DurationMultiplier(0.8) // Five notes in the time of four
  // Add more as needed
}
