package de.htwg.scalamusic.model

import de.htwg.scalamusic.{Octave, Volume}

sealed trait PitchClass

object PitchClass {
  case object C extends PitchClass
  case object CSharp extends PitchClass { override def toString: String = "C#" }
  case object D extends PitchClass
  case object DSharp extends PitchClass { override def toString: String = "D#" }
  case object E extends PitchClass
  case object F extends PitchClass
  case object FSharp extends PitchClass { override def toString: String = "F#" }
  case object G extends PitchClass
  case object GSharp extends PitchClass { override def toString: String = "G#" }
  case object A extends PitchClass
  case object ASharp extends PitchClass { override def toString: String = "A#" }
  case object B extends PitchClass

  val all: Seq[PitchClass] = Seq(C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B)
}

case class Pitch(pitchClass: PitchClass, octave: Octave) {
  override def toString: String = s"$pitchClass$octave"
  
  def +(semitones: Int): Pitch = {
    val currentIndex = PitchClass.all.indexOf(pitchClass)
    val totalSemitones = currentIndex + semitones
    // Custom modulo that works with negative numbers
    val newIndex = ((totalSemitones % 12) + 12) % 12
    val octaveShift = if (totalSemitones < 0) (totalSemitones - 11) / 12 else totalSemitones / 12
    Pitch(PitchClass.all(newIndex), octave + octaveShift)
  }
  
  def -(semitones: Int): Pitch = this + (-semitones)
}

object Pitch {
  // Helper methods for common pitches
  def C(octave: Int): Pitch = Pitch(PitchClass.C, octave)
  def D(octave: Int): Pitch = Pitch(PitchClass.D, octave)
  def E(octave: Int): Pitch = Pitch(PitchClass.E, octave)
  def F(octave: Int): Pitch = Pitch(PitchClass.F, octave)
  def G(octave: Int): Pitch = Pitch(PitchClass.G, octave)
  def A(octave: Int): Pitch = Pitch(PitchClass.A, octave)
  def B(octave: Int): Pitch = Pitch(PitchClass.B, octave)
  
  // Middle C is C4 in scientific pitch notation
  val middleC: Pitch = Pitch(PitchClass.C, 4)
}
