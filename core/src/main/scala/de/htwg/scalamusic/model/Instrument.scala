package de.htwg.scalamusic.model

/**
 * General MIDI Level 1 Instrument Patch Map
 * These are the standard MIDI program numbers (0-127)
 */
object Instrument {
  // Piano
  val AcousticGrandPiano = 0
  val BrightAcousticPiano = 1
  val ElectricGrandPiano = 2
  val HonkyTonkPiano = 3
  val ElectricPiano1 = 4
  val ElectricPiano2 = 5
  val Harpsichord = 6
  val Clavinet = 7
  
  // Chromatic Percussion
  val Celesta = 8
  val Glockenspiel = 9
  val MusicBox = 10
  val Vibraphone = 11
  val Marimba = 12
  val Xylophone = 13
  val TubularBells = 14
  val Dulcimer = 15
  
  // Organ
  val DrawbarOrgan = 16
  val PercussiveOrgan = 17
  val RockOrgan = 18
  val ChurchOrgan = 19
  val ReedOrgan = 20
  val Accordion = 21
  val Harmonica = 22
  val TangoAccordion = 23
  
  // Guitar
  val AcousticGuitarNylon = 24
  val AcousticGuitarSteel = 25
  val ElectricGuitarJazz = 26
  val ElectricGuitarClean = 27
  val ElectricGuitarMuted = 28
  val OverdrivenGuitar = 29
  val DistortionGuitar = 30
  val GuitarHarmonics = 31
  
  // And many more...
  
  // Percussive
  val TinkleBell = 112
  val Agogo = 113
  val SteelDrums = 114
  val Woodblock = 115
  val TaikoDrum = 116
  val MelodicTom = 117
  val SynthDrum = 118
  val ReverseCymbal = 119
  
  // Sound Effects
  val GuitarFretNoise = 120
  val BreathNoise = 121
  val Seashore = 122
  val BirdTweet = 123
  val TelephoneRing = 124
  val Helicopter = 125
  val Applause = 126
  val Gunshot = 127
}

/**
 * Represents a MIDI instrument with program change and channel information
 */
case class MidiInstrument(
  program: Int,
  channel: Int = 0,  // Default to channel 0
  bank: Int = 0,     // Default to bank 0 (melodic)
  percussion: Boolean = false
) {
  require(channel >= 0 && channel < 16, "Channel must be between 0 and 15")
  require(program >= 0 && program < 128, "Program must be between 0 and 127")
  require(bank >= 0 && bank <= 16383, "Bank must be between 0 and 16383")
}

object MidiInstrument {
  // Create a custom instrument
  def apply(program: Int, channel: Int, bank: Int, percussion: Boolean): MidiInstrument = 
    new MidiInstrument(program, channel, bank, percussion)
    
  def apply(program: Int, channel: Int, bank: Int): MidiInstrument = 
    new MidiInstrument(program, channel, bank, channel == 9)
    
  def apply(program: Int, channel: Int): MidiInstrument = 
    new MidiInstrument(program, channel, 0, channel == 9)
    
  def apply(program: Int): MidiInstrument = 
    new MidiInstrument(program, 0, 0, false)
  
  // Default instrument (Acoustic Grand Piano)
  val Default: MidiInstrument = MidiInstrument(program = Instrument.AcousticGrandPiano)
  
  // Percussion channel (channel 9 is typically used for percussion in GM)
  val Percussion: MidiInstrument = MidiInstrument(program = 0, channel = 9, bank = 0, percussion = true)
  
  // Some common instrument presets
  val Piano: MidiInstrument = MidiInstrument(program = Instrument.AcousticGrandPiano)
  val ElectricPiano: MidiInstrument = MidiInstrument(program = Instrument.ElectricPiano1)
  val Organ: MidiInstrument = MidiInstrument(program = Instrument.ChurchOrgan)
  val Guitar: MidiInstrument = MidiInstrument(program = Instrument.AcousticGuitarNylon)
  val Bass: MidiInstrument = MidiInstrument(program = 33)  // Acoustic Bass
  val Violin: MidiInstrument = MidiInstrument(program = 40)
  val Cello: MidiInstrument = MidiInstrument(program = 42)
  val Trumpet: MidiInstrument = MidiInstrument(program = 56)
  val Trombone: MidiInstrument = MidiInstrument(program = 57)
  val Flute: MidiInstrument = MidiInstrument(program = 73)
  val Clarinet: MidiInstrument = MidiInstrument(program = 71)
  val Strings: MidiInstrument = MidiInstrument(program = 48)  // String Ensemble 1
}
