package de.htwg.scalamusic.midi

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import de.htwg.scalamusic.model.DurationValue._
import javax.sound.midi.{MidiUnavailableException, ShortMessage}

class MidiPlayerSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  
  var midiPlayer: MidiPlayer = _
  
  override def beforeEach(): Unit = {
    midiPlayer = new MidiPlayer()
    // Don't actually initialize the MIDI system in tests
  }
  
  override def afterEach(): Unit = {
    try { midiPlayer.close() } catch { case _: Throwable => () }
  }
  
  "MidiPlayer" should {
    "be able to play a sequence of notes" in {
      // Just verify that the play method can be called without throwing exceptions
      noException should be thrownBy {
        val notes = Seq(
          Note(Pitch.C(4), DurationValue.Quarter),
          Note(Pitch.E(4), DurationValue.Quarter),
          Note(Pitch.G(4), DurationValue.Quarter)
        )
        midiPlayer.playSequence(notes)
      }
    }
    
    "be able to play a single note" in {
      noException should be thrownBy {
        midiPlayer.playNote(Pitch.C(4), 500, 0.5) // Using milliseconds for duration
      }
    }
  }
  
  // Note: Actual MIDI playback is not tested as it requires a MIDI synthesizer
  // and would be an integration test rather than a unit test
    
  "closing" should {
    "not throw when called multiple times" in {
      noException should be thrownBy {
        midiPlayer.close()
        midiPlayer.close()  // Should be idempotent
        midiPlayer.close()
        midiPlayer.close()  // Should be idempotent
      }
    }
  }
  
  "MidiPlayer companion object" should {
    "provide a factory method" in {
      val player = MidiPlayer()
      player shouldBe a[MidiPlayer]
      player.close()
    }
  }
}
