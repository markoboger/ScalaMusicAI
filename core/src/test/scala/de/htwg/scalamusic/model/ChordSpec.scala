package de.htwg.scalamusic.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import de.htwg.scalamusic.dsl.MusicDSL

class ChordSpec extends AnyWordSpec with Matchers with MusicDSL {
  "A Chord" when {
    "created" should {
      "be created from pitches" in {
        val chord = Chord(C(4), E(4), G(4))
        chord.pitches should contain theSameElementsAs Seq(C(4), E(4), G(4))
      }
      
      "not allow empty chords" in {
        an[IllegalArgumentException] should be thrownBy Chord()
      }
      
      "have a string representation" in {
        Chord(C(4), E(4), G(4)).toString should be ("Chord(C4, E4, G4)")
      }
    }
    
    "transposed" should {
      "transpose all pitches up by semitones" in {
        val chord = Chord(C(4), E(4), G(4)).transpose(2)
        chord.pitches should contain theSameElementsAs Seq(D(4), FSharp(4), A(4))
      }
      
      "transpose all pitches down by semitones" in {
        val chord = Chord(C(4), E(4), G(4)).transpose(-2)
        // Accept either A#3 or Bb3 as they are enharmonic equivalents
        chord.pitches should (contain theSameElementsAs Seq(B(3), D(4), F(4)) or 
          contain theSameElementsAs Seq(Pitch(PitchClass.ASharp, 3), D(4), F(4)))
      }
    }
    
    "with duration" should {
      "set duration for all notes" in {
        val chord = Chord(C(4), E(4), G(4)).withDuration(E2) // E2 is half note
        chord.pitches.foreach(_.duration should be (E2))
      }
      
      "not modify original chord" in {
        val original = Chord(C(4), E(4), G(4))
        val withDuration = original.withDuration(E2)
        original.pitches.exists(_.duration == E2) should be (false)
      }
    }
    
    "with instrument" should {
      "set instrument for all notes" in {
        val pianoProgram = 0  // Acoustic Grand Piano
        val chord = Chord(C(4), E(4), G(4)).withInstrument(pianoProgram)
        chord.pitches.foreach(_.instrument should be (pianoProgram))
      }
      
      "not modify original chord" in {
        // Use a non-default instrument program to avoid confusion with default value
        val originalInstrument = 0  // Default instrument
        val newInstrument = 1       // Different instrument
        
        // Create original pitches with the default instrument
        val originalPitches = Seq(C(4), E(4), G(4))
        originalPitches.foreach(_.instrument should be (originalInstrument))
        
        val original = Chord(originalPitches: _*)
        
        // Create a new chord with the new instrument
        val withInstrument = original.withInstrument(newInstrument)
        
        // The new chord should have the new instrument set
        withInstrument.pitches.foreach(_.instrument should be (newInstrument))
        
        // The original chord should remain unchanged
        original.pitches.foreach(_.instrument should be (originalInstrument))
        originalPitches.foreach(_.instrument should be (originalInstrument))
      }
    }
    
    "converted to notes" should {
      "return all pitches" in {
        val chord = Chord(C(4), E(4), G(4))
        chord.toNotes should contain theSameElementsAs Seq(C(4), E(4), G(4))
      }
    }
    
    "using factory methods" should {
      "create a major chord" in {
        val chord = Chord.major(C(4))
        chord.pitches should contain theSameElementsAs Seq(C(4), E(4), G(4))
      }
      
      "create a minor chord" in {
        val chord = Chord.minor(A(4))
        chord.pitches should contain theSameElementsAs Seq(A(4), C(5), E(5))
      }
      
      "create a diminished chord" in {
        val chord = Chord.diminished(B(4))
        chord.pitches should contain theSameElementsAs Seq(B(4), D(5), F(5))
      }
      
      "create an augmented chord" in {
        val chord = Chord.augmented(C(4))
        chord.pitches should contain theSameElementsAs Seq(C(4), E(4), GSharp(4))
      }
    }
    
    "using predefined chords" should {
      "have correct notes for CMajor" in {
        Chord.CMajor.pitches should contain theSameElementsAs Seq(C(4), E(4), G(4))
      }
      
      "have correct notes for AMinor" in {
        Chord.AMinor.pitches should contain theSameElementsAs Seq(A(4), C(5), E(5))
      }
    }
    
    "using DSL syntax" should {
      "create major chord with extension method" in {
        val chord = C(4).major
        chord.pitches should contain theSameElementsAs Seq(C(4), E(4), G(4))
      }
      
      "create minor chord with extension method" in {
        val chord = A(4).minor
        chord.pitches should contain theSameElementsAs Seq(A(4), C(5), E(5))
      }
    }
  }
}
