package de.htwg.scalamusic.dsl

import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import de.htwg.scalamusic.midi.MidiPlayer
import de.htwg.scalamusic.{Duration, Octave, Volume}
import scala.concurrent.duration._

/**
 * DSL for creating music in a more expressive way
 */
trait MusicDSL extends ChordDSL {
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
  
  // Lowercase note aliases with default octave (4)
  def c(octave: Int = 4): Pitch = C(octave)
  def cSharp(octave: Int = 4): Pitch = CSharp(octave)
  def d(octave: Int = 4): Pitch = D(octave)
  def dSharp(octave: Int = 4): Pitch = DSharp(octave)
  def e(octave: Int = 4): Pitch = E(octave)
  def f(octave: Int = 4): Pitch = F(octave)
  def fSharp(octave: Int = 4): Pitch = FSharp(octave)
  def g(octave: Int = 4): Pitch = G(octave)
  def gSharp(octave: Int = 4): Pitch = GSharp(octave)
  def a(octave: Int = 4): Pitch = A(octave)
  def aSharp(octave: Int = 4): Pitch = ASharp(octave)
  def b(octave: Int = 4): Pitch = B(octave)
  
  // Alias for natural notes without octave (defaults to 4)
  val c: Pitch = c()
  val d: Pitch = d()
  val eNote: Pitch = e()  // Renamed from 'e' to 'eNote' to avoid conflict with duration value
  val f: Pitch = f()
  val g: Pitch = g()
  val aNote: Pitch = a()  // Renamed from 'a' to 'aNote' to avoid conflict with Matchers trait
  val b: Pitch = b()
  
  // Aliases for backward compatibility
  @deprecated("Use CSharp instead", "1.0.0") def `C#`(octave: Int): Pitch = CSharp(octave)
  @deprecated("Use DSharp instead", "1.0.0") def `D#`(octave: Int): Pitch = DSharp(octave)
  @deprecated("Use FSharp instead", "1.0.0") def `F#`(octave: Int): Pitch = FSharp(octave)
  @deprecated("Use GSharp instead", "1.0.0") def `G#`(octave: Int): Pitch = GSharp(octave)
  @deprecated("Use ASharp instead", "1.0.0") def `A#`(octave: Int): Pitch = ASharp(octave)
  
  // Duration values (using numbers to avoid conflicts with note names)
  // The number represents the fraction of a whole note (e.g., E1 = whole, E2 = half, etc.)
  val E1 = DurationValue.Whole     // Whole note (1/1)
  val E2 = DurationValue.Half      // Half note (1/2)
  val E4 = DurationValue.Quarter   // Quarter note (1/4)
  val E8 = DurationValue.Eighth    // Eighth note (1/8)
  val E16 = DurationValue.Sixteenth // Sixteenth note (1/16)
  
  // Legacy duration values (deprecated, use E1, E2, E4, etc. instead)
  @deprecated("Use E1 instead", "1.0.0") val w: DurationValue = E1
  @deprecated("Use E2 instead", "1.0.0") val h: DurationValue = E2
  @deprecated("Use E4 instead", "1.0.0") val q: DurationValue = E4
  @deprecated("Use E8 instead", "1.0.0") val e: DurationValue = E8
  @deprecated("Use E16 instead", "1.0.0") val s: DurationValue = E16
  
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
