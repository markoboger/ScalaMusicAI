package de.htwg

package object scalamusic {
  // Type aliases for better readability
  type Octave = Int
  type BPM = Int
  
  // Duration in terms of whole notes (1.0 = whole note, 0.25 = quarter note, etc.)
  type Duration = Double
  
  // Volume (0.0 to 1.0)
  type Volume = Double
}
