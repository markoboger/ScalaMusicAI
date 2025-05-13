package de.htwg.scalamusic.dsl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import de.htwg.scalamusic.model.DurationValue._

class MusicDSLSpec extends AnyWordSpec with Matchers with MusicDSL {
  
  "MusicDSL" when {
    "creating notes" should {
      "provide pitch constructors for all notes" in {
        C(4) shouldBe Pitch.C(4)
        CSharp(4) shouldBe Pitch(PitchClass.CSharp, 4)
        D(4) shouldBe Pitch.D(4)
        DSharp(4) shouldBe Pitch(PitchClass.DSharp, 4)
        E(4) shouldBe Pitch.E(4)
        F(4) shouldBe Pitch.F(4)
        FSharp(4) shouldBe Pitch(PitchClass.FSharp, 4)
        G(4) shouldBe Pitch.G(4)
        GSharp(4) shouldBe Pitch(PitchClass.GSharp, 4)
        A(4) shouldBe Pitch.A(4)
        ASharp(4) shouldBe Pitch(PitchClass.ASharp, 4)
        B(4) shouldBe Pitch.B(4)
      }
      
      "provide duration value constants" in {
        w shouldBe Whole
        h shouldBe Half
        q shouldBe Quarter
        e shouldBe Eighth
        s shouldBe Sixteenth
      }
      
      "create notes with duration using * operator" in {
        val note = C(4) * q
        note shouldBe a[Note]
        note.pitch shouldBe Pitch.C(4)
        note.duration shouldBe Quarter
      }
      
      "create notes with tuplets using * operator" in {
        val tripletNote = C(4) * (q * (2.0 / 3.0))
        tripletNote.duration.value shouldBe (Quarter.value * (2.0 / 3.0) +- 0.001)
      }
      
      "create notes with articulation using withArticulation" in {
        val staccatoNote = E(4) * q withArticulation Articulation.Staccato
        staccatoNote.articulation shouldBe Some(Articulation.Staccato)
        
        val legatoNote = F(4) * h withArticulation Articulation.Legato
        legatoNote.articulation shouldBe Some(Articulation.Legato)
      }
      
      "create notes with custom volume using withVolume" in {
        val loudNote = G(4) * q withVolume 0.9
        loudNote.volume shouldBe 0.9
      }
      
      "create notes with instruments using withInstrument" in {
        val guitarNote = A(4) * q withInstrument MidiInstrument.Guitar
        guitarNote.instrument shouldBe MidiInstrument.Guitar
      }
    }
    
    "creating rests" should {
      "create rests with specified duration" in {
        val r = Rest(q)
        r shouldBe a[Rest]
        r.duration shouldBe Quarter
      }
    }
    
    "creating time signatures" should {
      "create time signatures with numerator and denominator" in {
        val ts = timeSig(3, 4)
        ts.numerator shouldBe 3
        ts.denominator shouldBe 4
      }
    }
    
    "creating tempos" should {
      "create tempos with BPM and note value" in {
        val tempo = bpm(120, q)
        tempo.bpm shouldBe 120
        tempo.noteValue shouldBe Quarter
      }
      
      "default to quarter note" in {
        val tempo = bpm(120)
        tempo.noteValue shouldBe Quarter
      }
    }
    
    "creating measures" should {
      "create measures with elements" in {
        val m = measure(C(4) * q, D(4) * q, E(4) * q, F(4) * q)
        m.elements should have size 4
        m.timeSignature.numerator shouldBe 4  // Default time signature
        m.tempo.bpm shouldBe 120  // Default tempo
      }
      
      "accept custom time signature and tempo" in {
        implicit val ts = timeSig(3, 4)
        implicit val tempo = bpm(90, h)
        
        val m = measure(C(4) * q, D(4) * q, E(4) * q)
        m.timeSignature shouldBe ts
        m.tempo shouldBe tempo
      }
    }
    
    "creating scores" should {
      "create scores from measures" in {
        val m1 = measure(C(4) * q, D(4) * q, E(4) * q, F(4) * q)
        val m2 = measure(G(4) * h, F(4) * q, E(4) * q)
        
        val s = score(m1, m2)
        s.measures should contain theSameElementsInOrderAs Seq(m1, m2)
      }
    }
  }
}
