package de.htwg.scalamusic.model

import de.htwg.scalamusic.model.DurationValue._
import de.htwg.scalamusic.model.Instrument._

/**
 * Represents a musical chord consisting of multiple pitches played simultaneously.
 *
 * @param pitches The sequence of pitches that make up the chord
 */
case class Chord(pitches: Pitch*) {
  require(pitches.nonEmpty, "A chord must contain at least one pitch")

  /**
   * Returns a new chord with all pitches transposed by the specified number of semitones.
   *
   * @param semitones The number of semitones to transpose the chord by
   * @return A new transposed chord
   */
  def transpose(semitones: Int): Chord = {
    Chord(pitches.map(_ + semitones): _*)
  }

  /**
   * Returns a new chord with all pitches having the specified duration.
   *
   * @param duration The duration to set for all notes in the chord
   * @return A new chord with the specified duration
   */
  def withDuration(duration: DurationValue): Chord = {
    Chord(pitches.map(_.withDuration(duration)): _*)
  }

  /**
   * Returns a new chord with all notes having the specified instrument.
   *
   * @param instrument The instrument to set for all notes in the chord
   * @return A new chord with the specified instrument
   */
  def withInstrument(instrument: Int): Chord = {
    Chord(pitches.map(_.withInstrument(instrument)): _*)
  }

  /**
   * Converts the chord to a sequence of notes.
   *
   * @return A sequence of notes representing the chord
   */
  def toNotes: Seq[Pitch] = pitches.toSeq.toSeq

  /**
   * Returns the duration of the first note in the chord, or Whole if empty.
   *
   * @return The duration of the chord
   */
  def duration: DurationValue = pitches.headOption.map(_.duration).getOrElse(DurationValue.Whole)

  /**
   * Returns the string representation of the chord.
   *
   * @return A string representation of the chord
   */
  override def toString: String = s"Chord(${pitches.mkString(", ")})"
}

/**
 * Companion object for the Chord class, providing factory methods.
 */
object Chord {
  // Factory methods for common chord types
  
  /**
   * Creates a major chord with the given root note.
   *
   * @param root The root note of the chord
   * @return A major chord
   */
  def major(root: Pitch): Chord = {
    val third = (root + 4).withDuration(root.duration).withInstrument(root.instrument)  // Major third is 4 semitones up
    val fifth = (root + 7).withDuration(root.duration).withInstrument(root.instrument) // Perfect fifth is 7 semitones up
    Chord(root, third, fifth)
  }

  // Factory method for creating a minor chord
  def minor(root: Pitch): Chord = {
    val third = (root + 3).withDuration(root.duration).withInstrument(root.instrument)  // Minor third is 3 semitones up
    val fifth = (root + 7).withDuration(root.duration).withInstrument(root.instrument) // Perfect fifth is 7 semitones up
    Chord(root, third, fifth)
  }

  // Factory method for creating a diminished chord
  def diminished(root: Pitch): Chord = {
    val third = (root + 3).withDuration(root.duration).withInstrument(root.instrument)  // Minor third is 3 semitones up
    val fifth = (root + 6).withDuration(root.duration).withInstrument(root.instrument) // Diminished fifth is 6 semitones up
    Chord(root, third, fifth)
  }

  // Factory method for creating an augmented chord
  def augmented(root: Pitch): Chord = {
    val third = (root + 4).withDuration(root.duration).withInstrument(root.instrument)  // Major third is 4 semitones up
    val fifth = (root + 8).withDuration(root.duration).withInstrument(root.instrument) // Augmented fifth is 8 semitones up
    Chord(root, third, fifth)
  }

  // Predefined major chords
  def CMajor: Chord = major(Pitch(PitchClass.C, 4))  // C4
  def DMajor: Chord = major(Pitch(PitchClass.D, 4))  // D4
  def EMajor: Chord = major(Pitch(PitchClass.E, 4))  // E4
  def FMajor: Chord = major(Pitch(PitchClass.F, 4))  // F4
  def GMajor: Chord = major(Pitch(PitchClass.G, 4))  // G4
  def AMajor: Chord = major(Pitch(PitchClass.A, 4))  // A4
  def BMajor: Chord = major(Pitch(PitchClass.B, 4)) // B4
  
  // Predefined minor chords
  def CMinor: Chord = minor(Pitch(PitchClass.C, 4))  // C4
  def DMinor: Chord = minor(Pitch(PitchClass.D, 4))  // D4
  def EMinor: Chord = minor(Pitch(PitchClass.E, 4))  // E4
  def FMinor: Chord = minor(Pitch(PitchClass.F, 4))  // F4
  def GMinor: Chord = minor(Pitch(PitchClass.G, 4))  // G4
  def AMinor: Chord = minor(Pitch(PitchClass.A, 4))  // A4
  def BMinor: Chord = minor(Pitch(PitchClass.B, 4)) // B4
}
