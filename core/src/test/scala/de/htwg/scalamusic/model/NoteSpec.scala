package de.htwg.scalamusic.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.scalamusic.Duration

class NoteSpec extends AnyWordSpec with Matchers {
  import DurationValue._
  
  "A Note" when {
    "created" should {
      "have default values" in {
        val note = Note(Pitch.C(4), Quarter)
        note.pitch shouldBe Pitch.C(4)
        note.duration shouldBe Quarter
        note.volume shouldBe 0.7
        note.articulation shouldBe None
        note.instrument shouldBe MidiInstrument.Default
      }
      
      "accept custom values" in {
        val articulation = Articulation.Staccato
        val instrument = MidiInstrument.Guitar
        val note = Note(
          pitch = Pitch.D(4),
          duration = Half,
          volume = 0.8,
          articulation = Some(articulation),
          instrument = instrument
        )
        
        note.pitch shouldBe Pitch.D(4)
        note.duration shouldBe Half
        note.volume shouldBe 0.8
        note.articulation shouldBe Some(articulation)
        note.instrument shouldBe instrument
      }
    }
    
    "modified" should {
      "return a new note with updated duration" in {
        val note = Note(Pitch.C(4), Quarter)
        val dotted = note.dotted
        dotted.duration shouldBe Quarter.dotted
        
        val withNewDuration = note.withDuration(Half)
        withNewDuration.duration shouldBe Half
      }
      
      "return a new note with updated volume" in {
        val note = Note(Pitch.C(4), Quarter)
        val louder = note.withVolume(0.9)
        louder.volume shouldBe 0.9
      }
      
      "return a new note with updated articulation" in {
        val note = Note(Pitch.C(4), Quarter)
        val staccato = note.withArticulation(Articulation.Staccato)
        staccato.articulation shouldBe Some(Articulation.Staccato)
      }
      
      "return a new note with updated instrument" in {
        val note = Note(Pitch.C(4), Quarter)
        val withGuitar = note.withInstrument(MidiInstrument.Guitar)
        withGuitar.instrument shouldBe MidiInstrument.Guitar
      }
    }
  }
  
  "A Rest" when {
    "created" should {
      "have the specified duration" in {
        val rest = Rest(Quarter)
        rest.duration shouldBe Quarter
      }
      
      "be convertible to a dotted rest" in {
        val rest = Rest(Quarter).dotted
        rest.duration shouldBe Quarter.dotted
      }
      
      "be convertible to a rest with different duration" in {
        val rest = Rest(Quarter).withDuration(Half)
        rest.duration shouldBe Half
      }
    }
  }
  
  "TimeSignature" when {
    "created" should {
      "accept valid time signatures" in {
        val ts = TimeSignature(3, 4)
        ts.numerator shouldBe 3
        ts.denominator shouldBe 4
        ts.beatsPerMeasure shouldBe 3
        ts.beatDuration shouldBe DurationValue.Quarter
      }
      
      "reject invalid denominators" in {
        an [IllegalArgumentException] should be thrownBy TimeSignature(4, 3)
        an [IllegalArgumentException] should be thrownBy TimeSignature(4, 5)
      }
    }
  }
  
  "Tempo" when {
    "created" should {
      "calculate durations correctly" in {
        val tempo = Tempo(120, DurationValue.Quarter)
        tempo.bpm shouldBe 120
        tempo.noteValue shouldBe DurationValue.Quarter
        tempo.millisecondsPerBeat shouldBe 500.0 +- 0.001
        tempo.wholeNoteDurationMs shouldBe 2000.0 +- 0.001
        
        // Test with different note value
        val halfNoteTempo = Tempo(60, DurationValue.Half)
        halfNoteTempo.durationMs(DurationValue.Whole) shouldBe 2000.0 +- 0.001
      }
      
      "reject non-positive BPM" in {
        an [IllegalArgumentException] should be thrownBy Tempo(0, DurationValue.Quarter)
        an [IllegalArgumentException] should be thrownBy Tempo(-1, DurationValue.Quarter)
      }
    }
  }
}
