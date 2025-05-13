package de.htwg.scalamusic.dsl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import de.htwg.scalamusic.dsl._
import de.htwg.scalamusic.model.DurationValue._
import de.htwg.scalamusic.model.PitchClass._
import scala.language.postfixOps

class MusicMonadsSpec extends AnyWordSpec with Matchers {
  "MusicMonad" should {
    "support sequencing of notes" in {
      val builder = new MusicBuilder[Id]()
      val program = for {
        _ <- builder.withTempo(120)
        _ <- builder.withInstrument(MidiInstrument.Piano)
        _ <- builder.playNote(Pitch(C, 4), Quarter)  // Using Quarter for quarter note
        _ <- builder.playNote(Pitch(E, 4), Quarter)  // Using Quarter for quarter note
      } yield ()

      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      // Verify the final context state
      finalCtx.instrument shouldBe MidiInstrument.Piano
      finalCtx.tempo shouldBe 120
    }
    
    "support the play method with default quarter note duration" in {
      val builder = new MusicBuilder[Id]()
      val program = for {
        _ <- builder.withTempo(120)
        _ <- builder.withInstrument(MidiInstrument.Piano)
        _ <- builder.play(Pitch(C, 4))  // Default quarter note
        _ <- builder.play(Pitch(E, 4), Half)  // Explicit half note
      } yield (())
      
      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      // Verify the final context state
      finalCtx.instrument shouldBe MidiInstrument.Piano
      finalCtx.tempo shouldBe 120
    }
    
    "support octave control in the monadic context" in {
      val builder = new MusicBuilder[Id]()
      
      val program = for {
        _ <- builder.withTempo(120)
        _ <- builder.withInstrument(MidiInstrument.Piano)
        // Play some notes with different octaves
        _ <- builder.play(Pitch(C, 4))  // C4
        _ <- builder.play(Pitch(D, 4))  // D4
        _ <- builder.play(Pitch(E, 4))  // E4
        _ <- builder.play(Pitch(F, 4))  // F4
        _ <- builder.play(Pitch(G, 4), Half)  // G4 with half duration
        _ <- builder.rest(Quarter)  // Quarter rest
        // Play with octave changes
        _ <- builder.play(Pitch(G, 4))  // G4
        _ <- builder.play(Pitch(A, 4))  // A4
        _ <- builder.play(Pitch(B, 4))  // B4
        _ <- builder.play(Pitch(C, 5))  // C5 (one octave up)
        _ <- builder.play(Pitch(D, 6), Half)  // D6 (two octaves up) with half duration
        _ <- builder.play(Pitch(C, 3), Quarter)  // C3 (one octave down)
      } yield (())
      
      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      // Verify the final context state
      finalCtx.instrument shouldBe MidiInstrument.Piano
      finalCtx.tempo shouldBe 120
    }

    "maintain context across operations" in {
      val builder = new MusicBuilder[Id]()
      val program = for {
        _ <- builder.withTempo(100)
        _ <- builder.withInstrument(MidiInstrument.Guitar)
        _ <- builder.withVolume(0.8)
        _ <- builder.withTimeSignature(TimeSignature(3, 4))
        _ <- builder.withKey(Key.GMajor)
      } yield ()

      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      finalCtx.tempo shouldBe 100
      finalCtx.instrument shouldBe MidiInstrument.Guitar
      finalCtx.volume shouldBe 0.8
      finalCtx.timeSignature shouldBe TimeSignature(3, 4)
      finalCtx.key shouldBe Key.GMajor
    }

    "support rest operations" in {
      val builder = new MusicBuilder[Id]()
      val program = for {
        _ <- builder.withTempo(120)
        _ <- builder.rest(Half)  // Half note rest
      } yield (())

      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      // Verify the final context state
      finalCtx.instrument shouldBe MidiInstrument.Piano  // Default instrument
      finalCtx.tempo shouldBe 120
    }

    "sequence multiple operations correctly" in {
      val builder = new MusicBuilder[Id]()
      val program = for {
        _ <- builder.withTempo(120)
        _ <- builder.sequence(
          builder.playNote(Pitch(C, 4), Quarter),  // Quarter note
          builder.playNote(Pitch(E, 4), Quarter),  // Quarter note
          builder.playNote(Pitch(G, 4), Quarter)   // Quarter note
        )
      } yield (())

      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      // Verify the final context state
      finalCtx.instrument shouldBe MidiInstrument.Piano  // Default instrument
      finalCtx.tempo shouldBe 120
    }
  }

  "MusicContext" should {
    "be updated correctly" in {
      val ctx = MusicContext()
        .withTempo(100)
        .withInstrument(MidiInstrument.Piano)
        .withVolume(0.9)
        .withTimeSignature(TimeSignature(6, 8))
        .withKey(Key.AMinor)
        .advancePosition(480)
      
      ctx.tempo shouldBe 100
      ctx.instrument shouldBe MidiInstrument.Piano
      ctx.volume shouldBe 0.9
      ctx.timeSignature shouldBe TimeSignature(6, 8)
      ctx.key shouldBe Key.AMinor
      ctx.position shouldBe 480
    }
  }

  // Id monad tests are removed as Id is now just a type alias

  "MusicBuilder" should {
    "create a valid music program" in {
      val builder = new MusicBuilder[Id]()
      val program = builder.sequence(
        builder.withTempo(120),
        builder.withInstrument(MidiInstrument.Piano),
        builder.playNote(Pitch.C(4), DurationValue.Quarter),
        builder.playNote(Pitch.E(4), DurationValue.Quarter)
      )
      
      val result = builder.runMusic(program, MusicContext())
      val (finalCtx, _) = result.value
      
      finalCtx.tempo shouldBe 120
      finalCtx.instrument shouldBe MidiInstrument.Piano
      finalCtx.position shouldBe 960 // 2 quarter notes
    }
  }
}
