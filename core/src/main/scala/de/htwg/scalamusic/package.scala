package de.htwg

import de.htwg.scalamusic.model.{Chord, Pitch}

package object scalamusic {
  // Type aliases for better readability
  type Octave = Int
  type BPM = Int
  
  // Duration in terms of whole notes (1.0 = whole note, 0.25 = quarter note, etc.)
  type Duration = Double
  
  // Volume (0.0 to 1.0)
  type Volume = Double
  
  /**
   * Extension methods for Pitch to create chords.
   */
  implicit class ChordExtensions(val root: Pitch) extends AnyVal {
    def major: Chord = de.htwg.scalamusic.model.Chord.major(root)
    def minor: Chord = de.htwg.scalamusic.model.Chord.minor(root)
    def diminished: Chord = de.htwg.scalamusic.model.Chord.diminished(root)
    def augmented: Chord = de.htwg.scalamusic.model.Chord.augmented(root)
    
    // Alias for major chord
    def maj: Chord = major
    def min: Chord = minor
    def dim: Chord = diminished
    def aug: Chord = augmented
  }
}
