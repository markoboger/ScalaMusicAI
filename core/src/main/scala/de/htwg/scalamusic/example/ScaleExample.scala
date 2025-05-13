package de.htwg.scalamusic.example

import de.htwg.scalamusic.dsl.MusicDSL
import de.htwg.scalamusic.model._

/**
 * Example of using the MusicDSL to create a C major scale
 */
object ScaleExample extends App with MusicDSL {
  // Import duration values for easier access
  import de.htwg.scalamusic.dsl.MusicDSL._
  
  // Define a simple C major scale
  val cMajorScale = Seq(
    C(4).withDuration(q),
    D(4).withDuration(q),
    E(4).withDuration(q),
    F(4).withDuration(q),
    G(4).withDuration(q),
    A(4).withDuration(q),
    B(4).withDuration(q),
    C(5).withDuration(q)
  )
  
  // Create a measure with the scale
  val scaleMeasure = measure(cMajorScale: _*)
  
  // Create a score with the measure
  val score = Score(Seq(scaleMeasure))
  
  // Print the score
  println("C Major Scale:")
  score.measures.foreach { measure =>
    println(s"Measure (${measure.timeSignature.numerator}/${measure.timeSignature.denominator}):")
    measure.elements.foreach {
      case Note(pitch, duration, _, _, _) => 
        println(s"  $pitch (${duration.value})")
      case Rest(duration) => 
        println(s"  Rest (${duration.value})")
    }
  }
  
  // Play the scale with default piano
  println("\nPlaying C Major Scale with Piano...")
  play(score)
  
  // Play the scale with different instruments
  println("\nPlaying C Major Scale with different instruments...")
  val instruments = Seq(
    ("Piano", MidiInstrument.Piano),
    ("Guitar", MidiInstrument.Guitar),
    ("Flute", MidiInstrument.Flute),
    ("Trumpet", MidiInstrument.Trumpet)
  )
  
  for ((name, instrument) <- instruments) {
    println(s"\nPlaying with $name...")
    val instrumentScore = score.copy(measures = score.measures.map { measure =>
      measure.copy(elements = measure.elements.map {
        case note: Note => note.withInstrument(instrument)
        case rest => rest
      })
    })
    play(instrumentScore)
    Thread.sleep(500)
  }
  
  // Play individual notes with different instruments
  println("\nPlaying individual notes with different instruments...")
  playNote(C(4), q, 0.7, MidiInstrument.Piano)
  playNote(E(4), q, 0.7, MidiInstrument.Guitar)
  playNote(G(4), h, 0.7, MidiInstrument.Trumpet)
}
