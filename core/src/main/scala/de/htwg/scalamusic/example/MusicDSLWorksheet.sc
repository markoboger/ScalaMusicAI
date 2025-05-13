import de.htwg.scalamusic.dsl._
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Note._
import de.htwg.scalamusic.model.DurationValue._
import de.htwg.scalamusic.model.Instrument._

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
val (finalCtx, _) = runMusic(melody, MusicContext())

// Print the final context to see the state
println(s"Final position: ${finalCtx.position} ticks")
println(s"Final tempo: ${finalCtx.tempo} BPM")
println(s"Final instrument: ${finalCtx.instrument}")

// You can also create more complex compositions
val chordProgression = for {
  _ <- withTempo(100)
  _ <- withInstrument(MidiInstrument.AcousticGrandPiano)

  // Play a C major chord (C, E, G)
  _ <- sequence(
    playNote(C4, Whole),
    playNote(E4, Whole),
    playNote(G4, Whole)
  )

  // Play an F major chord (F, A, C)
  _ <- sequence(
    playNote(F4, Whole),
    playNote(A4, Whole),
    playNote(C5, Whole)
  )

  // Play a G major chord (G, B, D)
  _ <- sequence(
    playNote(G4, Whole),
    playNote(B4, Whole),
    playNote(D5, Whole)
  )

  // Resolve back to C major
  _ <- sequence(
    playNote(C4, Whole),
    playNote(E4, Whole),
    playNote(G4, Whole)
  )
} yield ()

// Run the chord progression
val (chordCtx, _) = runMusic(chordProgression, MusicContext())
println(s"Chord progression ended at position: ${chordCtx.position} ticks")

// Helper function to play a scale
def playScale(root: Pitch, scaleType: String = "major"): MusicT[Id, Unit] = {
  val steps = scaleType match {
    case "major" => List(0, 2, 4, 5, 7, 9, 11, 12) // Major scale intervals
    case "minor" =>
      List(0, 2, 3, 5, 7, 8, 10, 12) // Natural minor scale intervals
    case _ => List(0, 2, 4, 5, 7, 9, 11, 12) // Default to major
  }

  val scaleNotes = steps.map(interval => root + interval)

  for {
    _ <- withTempo(100)
    _ <- withInstrument(MidiInstrument.Xylophone)
    _ <- sequence(scaleNotes.map(note => playNote(note, Eighth)): _*)
  } yield ()
}

// Play a C major scale using the simplified syntax
val scaleCtx = runMusic(playScale(c, "major"), MusicContext())._1
println(s"C major scale ended at position: ${scaleCtx.position} ticks")

// Play an A minor scale (using a3 explicitly to show it's still possible)
val aMinorCtx = runMusic(playScale(a - 1, "minor"), MusicContext())._1  // a3 is one octave below default a4
println(s"A minor scale ended at position: ${aMinorCtx.position} ticks")

// Example using the new octave control syntax and duration values
val simpleMelody = for {
  _ <- withTempo(100)
  _ <- withInstrument(MidiInstrument.AcousticGuitar)
  _ <- play(c)       // c4 with quarter note (default)
  _ <- play(e)       // e4 with quarter note (default)
  _ <- play(g)       // g4 with quarter note (default)
  _ <- play(c', E2)  // c5 with half note (using ' for one octave up)
} yield ()
val (simpleCtx, _) = runMusic(simpleMelody, MusicContext())
println(s"Simple melody ended at position: ${simpleCtx.position} ticks")

// Example of mixing octave controls and durations
val mixedMelody = for {
  _ <- withTempo(120)
  _ <- withInstrument(MidiInstrument.Violin)
  // Play a simple arpeggio with different octaves and durations
  _ <- play(c)                    // c4 with quarter note (default)
  _ <- play(e)                    // e4 with quarter note (default)
  _ <- play(g)                    // g4 with quarter note (default)
  _ <- play(c.octaveUp, Half)     // c5 with half note
  _ <- play(g.octaveUp, Quarter)   // g5 with quarter note
  _ <- play(e.octaveUp, Quarter)   // e5 with quarter note
  _ <- play(c.octaveUp, Half)      // c5 with half note
  _ <- play(g, Quarter)            // g4 with quarter note
  _ <- play(e, Quarter)            // e4 with quarter note
  _ <- play(c, Whole)              // c4 with whole note
} yield ()

val (mixedCtx, _) = runMusic(mixedMelody, MusicContext())
println(s"Mixed melody ended at position: ${mixedCtx.position} ticks")

// Example using the new duration values
val durationDemo = for {
  _ <- withTempo(80)
  _ <- withInstrument(MidiInstrument.Piano)
  // Play the same note with different durations
  _ <- play(c, Whole)            // Whole note (E1)
  _ <- play(c, Half)             // Half note (E2)
  _ <- play(c, Quarter)           // Quarter note (E4)
  _ <- play(c, Eighth)            // Eighth note (E8)
  _ <- play(c, Sixteenth)         // Sixteenth note (E16)
} yield ()
val (durationCtx, _) = runMusic(durationDemo, MusicContext())
println(s"Duration demo ended at position: ${durationCtx.position} ticks")
