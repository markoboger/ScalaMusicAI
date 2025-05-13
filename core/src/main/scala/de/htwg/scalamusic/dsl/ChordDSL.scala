package de.htwg.scalamusic.dsl

import de.htwg.scalamusic.model.{Chord, Pitch, PitchClass}

/**
 * DSL extensions for chord creation and manipulation.
 */
trait ChordDSL { self: MusicDSL =>
  
  /**
   * Creates a chord from the given pitches.
   * 
   * @param pitches The pitches that make up the chord
   * @return A new Chord instance
   */
  def makeChord(pitches: Pitch*): Chord = new de.htwg.scalamusic.model.Chord(pitches: _*)
  
  /**
   * Creates a major chord with the given root note.
   * 
   * @param root The root note of the chord
   * @return A major chord
   */
  def major(root: Pitch): Chord = de.htwg.scalamusic.model.Chord.major(root)
  
  /**
   * Creates a minor chord with the given root note.
   * 
   * @param root The root note of the chord
   * @return A minor chord
   */
  def minor(root: Pitch): Chord = de.htwg.scalamusic.model.Chord.minor(root)
  
  /**
   * Creates a diminished chord with the given root note.
   * 
   * @param root The root note of the chord
   * @return A diminished chord
   */
  def diminished(root: Pitch): Chord = de.htwg.scalamusic.model.Chord.diminished(root)
  
  /**
   * Creates an augmented chord with the given root note.
   * 
   * @param root The root note of the chord
   * @return An augmented chord
   */
  def augmented(root: Pitch): Chord = de.htwg.scalamusic.model.Chord.augmented(root)
  
  // Predefined chords for convenience
  
  // Major chords
  val CMajor: Chord = de.htwg.scalamusic.model.Chord.CMajor
  val DMajor: Chord = de.htwg.scalamusic.model.Chord.DMajor
  val EMajor: Chord = de.htwg.scalamusic.model.Chord.EMajor
  val FMajor: Chord = de.htwg.scalamusic.model.Chord.FMajor
  val GMajor: Chord = de.htwg.scalamusic.model.Chord.GMajor
  val AMajor: Chord = de.htwg.scalamusic.model.Chord.AMajor
  val BMajor: Chord = de.htwg.scalamusic.model.Chord.BMajor
  
  // Minor chords
  val CMinor: Chord = de.htwg.scalamusic.model.Chord.CMinor
  val DMinor: Chord = de.htwg.scalamusic.model.Chord.DMinor
  val EMinor: Chord = de.htwg.scalamusic.model.Chord.EMinor
  val FMinor: Chord = de.htwg.scalamusic.model.Chord.FMinor
  val GMinor: Chord = de.htwg.scalamusic.model.Chord.GMinor
  val AMinor: Chord = de.htwg.scalamusic.model.Chord.AMinor
  val BMinor: Chord = de.htwg.scalamusic.model.Chord.BMinor
}
