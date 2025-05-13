package de.htwg.scalamusic.model

import de.htwg.scalamusic.Volume

sealed trait MusicElement {
  def duration: DurationValue
}

case class Note(
  pitch: Pitch,
  duration: DurationValue,
  volume: Volume = 0.7,
  articulation: Option[Articulation] = None,
  instrument: MidiInstrument = MidiInstrument.Default
) extends MusicElement {
  def dotted: Note = copy(duration = duration.dotted)
  def withDuration(newDuration: DurationValue): Note = copy(duration = newDuration)
  def withVolume(newVolume: Volume): Note = copy(volume = newVolume)
  def withArticulation(art: Articulation): Note = copy(articulation = Some(art))
  def withInstrument(instrument: MidiInstrument): Note = copy(instrument = instrument)
}

case class Rest(duration: DurationValue) extends MusicElement {
  def dotted: Rest = Rest(duration.dotted)
  def withDuration(newDuration: DurationValue): Rest = copy(duration = newDuration)
}

// Articulations
trait Articulation
object Articulation {
  case object Staccato extends Articulation
  case object Legato extends Articulation
  case object Accent extends Articulation
  // Add more articulations as needed
}

// Time signature representation
case class TimeSignature(numerator: Int, denominator: Int) {
  require(denominator > 0 && (denominator & (denominator - 1)) == 0, 
    "Denominator must be a power of 2")
  
  def beatsPerMeasure: Int = numerator
  def beatDuration: DurationValue = DurationValue.fromDouble(1.0 / denominator)
}

// Tempo in beats per minute
case class Tempo(bpm: Int, noteValue: DurationValue = DurationValue.Quarter) {
  require(bpm > 0, "Tempo must be positive")
  
  def millisecondsPerBeat: Double = 60000.0 / bpm
  
  // Duration of a whole note in milliseconds
  def wholeNoteDurationMs: Double = millisecondsPerBeat * (1.0 / noteValue.value)
  
  // Duration of a specific note in milliseconds
  def durationMs(duration: DurationValue): Double = 
    wholeNoteDurationMs * duration.value
}

// A measure contains music elements that fit within its time signature
case class Measure(
  elements: Seq[MusicElement],
  timeSignature: TimeSignature,
  tempo: Tempo
) {
  // Add validation to ensure total duration doesn't exceed measure length
  // ...
}

// A score is a sequence of measures
case class Score(measures: Seq[Measure]) {
  def addMeasure(measure: Measure): Score = copy(measures = measures :+ measure)
  // Add more score manipulation methods as needed
}
