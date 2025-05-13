package de.htwg.scalamusic.dsl

import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Pitch
import de.htwg.scalamusic.model.PitchClass
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

// Import the DSL methods directly
import de.htwg.scalamusic.dsl.MusicDSL._

class MusicDSLSpec extends AnyWordSpec with Matchers {
  
  "The MusicDSL" should {
    "create notes with correct pitch and octave" in {
      val c4 = Pitch.C(4)
      c4.pitchClass shouldBe PitchClass.C
      c4.octave shouldBe 4
    }
    
    "support octave transposition" in {
      val c4 = Pitch.C(4)
      (c4 + 12).octave shouldBe 5
      (c4 - 12).octave shouldBe 3
    }
    
    "support octave transposition with operators" in {
      val c4 = Pitch.C(4)
      val c5 = Pitch.C(5)
      val c6 = Pitch.C(6)
      
      (c4 + 12).octave shouldBe 5
      (c5 - 12).octave shouldBe 4
      (c4 + 24).octave shouldBe 6
    }
    
    "create notes with duration" in {
      import de.htwg.scalamusic.model.DurationValue.Quarter
      val note = Note(Pitch.C(4), Quarter)
      note.pitch shouldBe Pitch.C(4)
      note.duration.value shouldBe 0.25  // Quarter note has value 0.25 of a whole note
    }
    
    "create notes with duration using DSL" in {
      import de.htwg.scalamusic.model.DurationValue.Quarter
      // Use the Pitch.C method directly
      val note = Note(Pitch.C(4), Quarter)
      note.pitch.pitchClass shouldBe PitchClass.C
      note.duration.value shouldBe 0.25  // Quarter note has value 0.25 of a whole note
    }
  }
}
