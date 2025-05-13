package de.htwg.scalamusic.example

import de.htwg.scalamusic.dsl.MusicDSL
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._

/**
 * Example demonstrating different instruments playing together
 */
object InstrumentDemo extends App with MusicDSL {
  // Import duration values for easier access
  import de.htwg.scalamusic.dsl.MusicDSL._
  
  // Define a simple melody (C major arpeggio)
  val melody = Seq(
    C(4).withDuration(q),
    E(4).withDuration(q),
    G(4).withDuration(q),
    C(5).withDuration(q)
  )
  
  // Play the melody with different instruments
  def playWithInstruments(): Unit = {
    println("\n=== Instrument Demo ===")
    
    // Piano
    println("\nPlaying with Piano...")
    play(melody.map(_.withInstrument(MidiInstrument.Piano)))
    Thread.sleep(500)
    
    // Guitar
    println("\nPlaying with Guitar...")
    play(melody.map(_.withInstrument(MidiInstrument.Guitar)))
    Thread.sleep(500)
    
    // Flute
    println("\nPlaying with Flute...")
    play(melody.map(_.withInstrument(MidiInstrument.Flute)))
    Thread.sleep(500)
    
    // Trumpet
    println("\nPlaying with Trumpet...")
    play(melody.map(_.withInstrument(MidiInstrument.Trumpet)))
    Thread.sleep(500)
  }
  
  // Play a chord progression with different instruments
  def playChordProgression(): Unit = {
    println("\n=== Chord Progression ===")
    
    // Define chords (C, G, Am, F)
    val cMajor = Seq(C(4), E(4), G(4))
    val gMajor = Seq(G(3), B(3), D(4))
    val aMinor = Seq(A(3), C(4), E(4))
    val fMajor = Seq(F(3), A(3), C(4))
    
    val chords = Seq(cMajor, gMajor, aMinor, fMajor)
    
    // Play each chord with a different instrument
    val instruments = Seq(
      MidiInstrument.Piano,
      MidiInstrument.Guitar,
      MidiInstrument.Organ,
      MidiInstrument.Strings
    )
    
    for ((chord, instrument) <- chords.zip(instruments)) {
      println(s"\nPlaying chord with ${instrument.getClass.getSimpleName}...")
      val notes = chord.map(note => note.withDuration(h).withInstrument(instrument))
      play(notes)
      Thread.sleep(1000)
    }
  }
  
  // Play a simple duet (melody and bass)
  def playDuet(): Unit = {
    println("\n=== Duet (Melody and Bass) ===")
    
    // Melody (right hand)
    val melodyNotes = Seq(
      C(5).withDuration(q),
      D(5).withDuration(e),
      E(5).withDuration(e),
      F(5).withDuration(q),
      G(5).withDuration(q),
      E(5).withDuration(q),
      C(5).withDuration(q),
      A(4).withDuration(h)
    ).map(_.withInstrument(MidiInstrument.Flute))
    
    // Bass (left hand)
    val bassNotes = Seq(
      C(3).withDuration(h),
      G(2).withDuration(h),
      A(2).withDuration(h),
      F(2).withDuration(h)
    ).map(_.withInstrument(MidiInstrument.Bass))
    
    // Play them together
    val allNotes = melodyNotes.zipAll(bassNotes, null, null).flatMap { 
      case (m, b) => Seq(Option(m), Option(b)).flatten 
    }
    
    play(allNotes, 100)
  }
  
  // Run the demos
  playWithInstruments()
  playChordProgression()
  playDuet()
  
  println("\nDemo complete!")
}
