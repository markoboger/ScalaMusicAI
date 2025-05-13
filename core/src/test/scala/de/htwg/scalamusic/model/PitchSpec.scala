package de.htwg.scalamusic.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class PitchSpec extends AnyWordSpec with Matchers {
  "A Pitch" when {
    "created" should {
      "have a string representation" in {
        Pitch(PitchClass.C, 4).toString should be("C4")
        Pitch(PitchClass.CSharp, 4).toString should be("C#4")
        Pitch(PitchClass.D, 4).toString should be("D4")
      }
      
      "be constructable from helper methods" in {
        Pitch.C(4) shouldBe Pitch(PitchClass.C, 4)
        Pitch.D(4) shouldBe Pitch(PitchClass.D, 4)
        Pitch.E(4) shouldBe Pitch(PitchClass.E, 4)
      }
      
      "support octave control with apostrophes and unary operators" in {
        val c4 = Pitch.C(4)
        c4.`'` shouldBe Pitch.C(5)    // One octave up
        c4.`''` shouldBe Pitch.C(6)   // Two octaves up
        c4.`'''` shouldBe Pitch.C(7)  // Three octaves up
        
        // Test unary operators
        val c5 = +c4
        c5 shouldBe Pitch.C(5)  // One octave up from C4
        
        val c3 = -c4
        c3 shouldBe Pitch.C(3)  // One octave down from C4
        
        val c2 = -c3
        c2 shouldBe Pitch.C(2)  // One octave down from C3
        
        // Test with different notes
        val g4 = Pitch.G(4)
        g4.`'` shouldBe Pitch.G(5)
        -g4 shouldBe Pitch.G(3)
      }
    }
    
    "transposed" should {
      "move up by the specified number of semitones" in {
        val c4 = Pitch.C(4)
        c4 + 2 shouldBe Pitch.D(4)
        c4 + 12 shouldBe Pitch.C(5)  // Octave up
      }
      
      "move down by the specified number of semitones" in {
        val c4 = Pitch.C(4)
        c4 - 2 shouldBe Pitch(PitchClass.ASharp, 3)
        c4 - 12 shouldBe Pitch.C(3)  // Octave down
      }
      
      "handle negative semitones correctly" in {
        val c4 = Pitch.C(4)
        c4 + (-2) shouldBe Pitch(PitchClass.ASharp, 3)
      }
      
      "wrap around the octave correctly" in {
        val b3 = Pitch.B(3)
        b3 + 1 shouldBe Pitch.C(4)
        
        val c4 = Pitch.C(4)
        c4 - 1 shouldBe Pitch.B(3)
      }
    }
  }
  
  "PitchClass" should {
    "contain all 12 chromatic notes" in {
      PitchClass.all should have size 12
    }
    
    "have correct string representations for accidentals" in {
      PitchClass.CSharp.toString should be ("C#")
      PitchClass.DSharp.toString should be ("D#")
      PitchClass.FSharp.toString should be ("F#")
      PitchClass.GSharp.toString should be ("G#")
      PitchClass.ASharp.toString should be ("A#")
    }
  }
}
