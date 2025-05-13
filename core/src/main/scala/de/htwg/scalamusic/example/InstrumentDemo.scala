package de.htwg.scalamusic.example

import de.htwg.scalamusic.dsl.MusicDSL
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.DurationValue._
import de.htwg.scalamusic.model.Pitch._
import de.htwg.scalamusic.model.Note
import de.htwg.scalamusic.model.MidiInstrument
import de.htwg.scalamusic.{Duration, Octave, Volume}
import scala.language.postfixOps

/**
 * Example demonstrating different instruments playing together
 */
object InstrumentDemo extends App with MusicDSL {
  // Import duration values
  import de.htwg.scalamusic.dsl.MusicDSL._
  
  // Import MidiInstrument presets with full qualification
  val Piano = MidiInstrument.Piano
  val Guitar = MidiInstrument.Guitar
  val Flute = MidiInstrument.Flute
  val Trumpet = MidiInstrument.Trumpet
  val Bass = MidiInstrument.Bass
  val Strings = MidiInstrument.Strings
  val Clarinet = MidiInstrument.Clarinet
  val Trombone = MidiInstrument.Trombone
  val Violin = MidiInstrument.Violin
  val Cello = MidiInstrument.Cello
  val Organ = MidiInstrument.Organ
  val ElectricPiano = MidiInstrument.ElectricPiano
  
  // Define a simple melody using a C major chord arpeggio
  val cMajor = Chord.major(C(4)).withDuration(E4)
  val melody = (cMajor.pitches :+ C(5)).map(p => Note(p, E4))  // C4, E4, G4, C5
  
  // Play the melody with different instruments
  def playWithInstruments(): Unit = {
    println("\n=== Instrument Demo ===")
    
    // Piano
    println("\nPlaying with Piano...")
    play(melody.map(_.withInstrument(Piano)), 120)
    Thread.sleep(500)
    
    // Play the full chord
    println("\nPlaying full C major chord...")
    play(cMajor.pitches.map(p => Note(p, E4).withInstrument(Piano)), 120)
    Thread.sleep(500)
    
    // Guitar
    println("\nPlaying with Guitar...")
    play(melody.map(_.withInstrument(Guitar)), 120)
    Thread.sleep(500)
    
    // Flute
    println("\nPlaying with Flute...")
    play(melody.map(_.withInstrument(Flute)), 120)
    Thread.sleep(500)
    
    // Trumpet
    println("\nPlaying with Trumpet...")
    play(melody.map(_.withInstrument(Trumpet)), 120)
    Thread.sleep(500)
  }
  
  // Play a chord progression with different instruments
  def playChordProgression(): Unit = {
    println("\n=== Chord Progression ===")
    
    // Define chords using the factory methods
    val cMajor = Chord.major(C(4))
    val gMajor = Chord.major(G(4))
    val aMinor = Chord.minor(A(4))
    val fMajor = Chord.major(F(4))
    
    // Or using the explicit syntax
    // val cMajor = Chord(C(4), E(4), G(4))
    // val gMajor = Chord(G(4), B(4), D(5))
    // val aMinor = Chord(A(4), C(5), E(5))
    // val fMajor = Chord(F(4), A(4), C(5))
    
    // Or using predefined chords
    // val cMajor = Chord.CMajor
    // val gMajor = Chord.GMajor
    // val aMinor = Chord.AMinor
    // val fMajor = Chord.FMajor
    
    val chords = Seq(cMajor, gMajor, aMinor, fMajor)
    
    // Play each chord with a different instrument
    val instruments = Seq(
      MidiInstrument.Piano,   // Piano
      MidiInstrument.Guitar,  // Guitar
      MidiInstrument.Organ,   // Organ
      MidiInstrument.Strings  // Strings
    )
    
    for ((chord, instrument) <- chords.zip(instruments)) {
      println(s"\nPlaying ${chord.toString} with instrument $instrument...")
      playChordWithInstruments(chord, E2, instrument)
      Thread.sleep(1000)
    }
  }
  
  def playChordWithInstruments(chord: Chord, duration: DurationValue, instrument: MidiInstrument): Unit = {
    val chordWithDuration = chord.withDuration(duration)
    play(chordWithDuration.pitches.map(p => Note(p, E2).withInstrument(instrument)), 120)
  }
  
  // Play a simple duet (melody and bass)
  def playDuet(): Unit = {
    println("\n=== Duet (Melody and Bass) ===")
    
    // Melody (right hand)
    val melodyNotes = Seq(
      C(5),
      D(5),
      E(5),
      F(5),
      G(5),
      E(5),
      C(5),
      A(4)
    ).map(p => Note(p, E4).withInstrument(MidiInstrument.Flute))
    
    // Bass (left hand)
    val bassNotes = Seq(
      C(3),
      G(2),
      A(2),
      F(2)
    ).map(p => Note(p, E2).withInstrument(Bass))
    
    // Play them together
    play(melodyNotes ++ bassNotes, 120)
  }
  
  // Play a chord progression with voice leading
  def playVoiceLeading(): Unit = {
    println("\n=== Voice Leading Example ===")
    
    // A simple I-IV-V-I progression with smooth voice leading
    val soprano = Seq(
      C(5),
      B(4),
      C(5),
      G(4)
    )
    
    val alto = Seq(
      E(4),
      C(4),
      G(4),
      E(4)
    )
    
    val tenor = Seq(
      G(3),
      A(3),
      E(4),
      C(4)
    )
    
    val bass = Seq(
      C(3),
      F(3),
      C(4),
      C(3)
    )
    
    // Combine voices with different instruments and convert to Notes
    val voices = Seq(
      soprano.map(p => Note(p, E4).withInstrument(MidiInstrument.Strings)),
      alto.map(p => Note(p, E4).withInstrument(MidiInstrument.Strings)),
      tenor.map(p => Note(p, E4).withInstrument(MidiInstrument.Strings)),
      bass.map(p => Note(p, E4).withInstrument(MidiInstrument.Bass))
    )
    
    // Play all voices together
    voices.foreach(notes => play(notes, 120))
  }
  
  // Play a more complex polyphonic piece
  def playPolyphonicPiece(): Unit = {
    println("\n=== Polyphonic Piece ===")
    
    // Melody (Soprano)
    val melody = Seq(
      C(5), D(5), E(5), F(5),
      G(5), A(5), G(5), F(5),
      E(5), D(5), C(5), D(5)
    )
    val melodyDurations = Seq(E4, E8, E8, E4, E4, E8, E8, E4, E4, E8, E8, E2)
    
    // Counter-melody (Alto)
    val counterMelody = Seq(
      E(4), F(4), G(4), A(4),
      B(4), C(5), B(4), A(4),
      G(4), F(4), E(4), D(4)
    )
    val counterDurations = Seq(E4, E4, E4, E4, E4, E8, E8, E4, E4, E8, E8, E2)
    
    // Chords (Tenor and Bass)
    val chords = Seq(
      (C(4), C(3)), (D(4), G(2)), (E(4), C(3)), (F(4), F(3)),
      (G(4), G(2)), (A(4), C(3)), (G(4), G(2)), (A(4), F(3)),
      (G(4), E(3)), (F(4), D(3)), (E(4), C(3)), (D(4), G(2))
    )
    
    // Prepare notes with durations and instruments
    val melodyNotes = melody.zip(melodyDurations).map { case (p, d) => 
      Note(p, d).withInstrument(MidiInstrument.Flute)
    }
    val counterNotes = counterMelody.zip(counterDurations).map { case (p, d) =>
      Note(p, d).withInstrument(MidiInstrument.Clarinet)
    }
    val chordNotes = chords.flatMap { case (top, bottom) => 
      Seq(
        Note(top, E4).withInstrument(MidiInstrument.Strings),
        Note(bottom, E4).withInstrument(MidiInstrument.Strings)
      )
    }
    
    // Play all parts together - simplify by concatenating all notes
    val allNotes = (melodyNotes ++ counterNotes ++ chordNotes).filter(_ != null)
    
    play(allNotes, 100)
  }
  
  // Run the demos
  playWithInstruments()
  playChordProgression()
  playDuet()
  playVoiceLeading()
  playPolyphonicPiece()
  
  println("\nDemo complete!")
}
