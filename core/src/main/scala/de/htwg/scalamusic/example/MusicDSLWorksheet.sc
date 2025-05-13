package de.htwg.scalamusic.example

import de.htwg.scalamusic.dsl._
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Note._
import de.htwg.scalamusic.model.DurationValue._
import de.htwg.scalamusic.model.Instrument._

object MusicDSLWorksheet extends App {
  // Create a music builder instance
  val builder = new MusicBuilder[Id]()
  import builder._

  // Define a simple melody using the new octave control syntax
  val melody = for {
    _ <- withTempo(120)
    _ <- withInstrument(MidiInstrument.Piano)
    // Using the new octave control with apostrophes and new duration values
    _ <- play(c)    // c4 with quarter note (default)
    _ <- play(d)    // d4 with quarter note (default)
    _ <- play(e)    // e4 with quarter note (default)
    _ <- play(f)    // f4 with quarter note (default)
    _ <- play(g, E2) // g4 with half note (explicit duration)
    _ <- rest(E4)   // Quarter rest
    // Octave control with apostrophes
    _ <- play(g)    // g4 with quarter note (default)
    _ <- play(a)    // a4 with quarter note (default)
    _ <- play(b)    // b4 with quarter note (default)
    _ <- play(c')   // c5 (one octave up) with quarter note (default)
    _ <- play(d'', E2) // d6 (two octaves up) with half note
  } yield ()
  
  // Run the music program
  println("Starting music playback...")
  val (finalCtx, _) = runMusic(melody, MusicContext())
  
  // Print the final context to see the state
  println(s"Final position: ${finalCtx.position} ticks")
  println(s"Final tempo: ${finalCtx.tempo} BPM")
  println(s"Final instrument: ${finalCtx.instrument}")
}
