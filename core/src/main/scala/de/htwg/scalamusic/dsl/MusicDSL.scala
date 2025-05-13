package de.htwg.scalamusic.dsl

import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import de.htwg.scalamusic.midi.MidiPlayer
import de.htwg.scalamusic.{Duration, Octave, Volume}
import scala.concurrent.duration._

/**
 * DSL for creating music in a more expressive way
 */
trait MusicDSL {
  // Note constructors
  def C(octave: Int): Pitch = Pitch.C(octave)
  def CSharp(octave: Int): Pitch = Pitch(PitchClass.CSharp, octave)
  def D(octave: Int): Pitch = Pitch.D(octave)
  def DSharp(octave: Int): Pitch = Pitch(PitchClass.DSharp, octave)
  def E(octave: Int): Pitch = Pitch.E(octave)
  def F(octave: Int): Pitch = Pitch.F(octave)
  def FSharp(octave: Int): Pitch = Pitch(PitchClass.FSharp, octave)
  def G(octave: Int): Pitch = Pitch.G(octave)
  def GSharp(octave: Int): Pitch = Pitch(PitchClass.GSharp, octave)
  def A(octave: Int): Pitch = Pitch.A(octave)
  def ASharp(octave: Int): Pitch = Pitch(PitchClass.ASharp, octave)
  def B(octave: Int): Pitch = Pitch.B(octave)
  
  // Aliases for backward compatibility
  @deprecated("Use CSharp instead", "1.0.0") def `C#`(octave: Int): Pitch = CSharp(octave)
  @deprecated("Use DSharp instead", "1.0.0") def `D#`(octave: Int): Pitch = DSharp(octave)
  @deprecated("Use FSharp instead", "1.0.0") def `F#`(octave: Int): Pitch = FSharp(octave)
  @deprecated("Use GSharp instead", "1.0.0") def `G#`(octave: Int): Pitch = GSharp(octave)
  @deprecated("Use ASharp instead", "1.0.0") def `A#`(octave: Int): Pitch = ASharp(octave)
  
  // Duration values
  val w = DurationValue.Whole
  val h = DurationValue.Half
  val q = DurationValue.Quarter
  val e = DurationValue.Eighth
  val s = DurationValue.Sixteenth
  
  // Tuplets
  val triplet = DurationMultiplier.Triplet
  val quintuplet = DurationMultiplier.Quintuplet
  
  // Articulations
  val staccato = Articulation.Staccato
  val legato = Articulation.Legato
  val accent = Articulation.Accent
  
  // Note creation with infix operators for common operations
  implicit class NoteBuilder(pitch: Pitch) {
    def *(duration: DurationValue): Note = Note(pitch, duration)
    
    def withDuration(duration: DurationValue): Note = Note(pitch, duration)
    
    // Use descriptive method names to avoid erasure conflicts
    def withMultiplier(duration: DurationValue, multiplier: DurationMultiplier): Note =
      Note(pitch, duration * multiplier.factor)
      
    def withArticulation(duration: DurationValue, articulation: Articulation): Note =
      Note(pitch, duration, articulation = Some(articulation))
      
    def withVolume(duration: DurationValue, volume: Volume): Note =
      Note(pitch, duration, volume = volume)
      
    def withInstrument(duration: DurationValue, instrument: MidiInstrument): Note =
      Note(pitch, duration, instrument = instrument)
      
    // For backward compatibility, keep the * operator but with a single type
    def *(duration: (DurationValue, DurationMultiplier)): Note = 
      withMultiplier(duration._1, duration._2)
  }
  
  // Rest creation
  def rest(duration: DurationValue): Rest = Rest(duration)
  
  // Time signatures
  def timeSig(numerator: Int, denominator: Int): TimeSignature = 
    TimeSignature(numerator, denominator)
    
  // Tempo
  def bpm(beats: Int, noteValue: DurationValue = DurationValue.Quarter): Tempo =
    Tempo(beats, noteValue)
    
  // Measure creation
  def measure(
    elements: MusicElement*
  )(implicit 
    ts: TimeSignature = TimeSignature(4, 4), 
    tempo: Tempo = Tempo(120)
  ): Measure = {
    Measure(elements, ts, tempo)
  }
  
  // Score creation
  def score(measures: Measure*): Score = Score(measures.toSeq)
  
  // Play a score using MIDI
  def play(score: Score): Unit = {
    MidiPlayer.play(score)
  }
  
  // Play a sequence of notes
  def play(notes: Seq[Note], tempo: Int = 120): Unit = {
    val player = MidiPlayer()
    try {
      player.playSequence(notes, tempo)
    } finally {
      player.close()
    }
  }
  
  // Play a single note (for testing)
  def playNote(
    pitch: Pitch, 
    duration: DurationValue, 
    volume: Volume = 0.7,
    instrument: MidiInstrument = MidiInstrument.Default
  ): Unit = {
    val player = MidiPlayer()
    try {
      val durationMs = (duration.value * 1000).toLong // Simple duration for single note
      player.playNote(pitch, durationMs, volume, instrument)
      Thread.sleep(durationMs / 1000) // Wait for note to finish
    } finally {
      player.close()
    }
  }
}

object MusicDSL extends MusicDSL
