package de.htwg.scalamusic.midi

import javax.sound.midi.{MidiSystem, MidiUnavailableException, ShortMessage, Receiver, Synthesizer}
import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import de.htwg.scalamusic.{Volume => Vol}
import scala.util.{Try, Success, Failure}
import scala.collection.mutable

class MidiPlayer {
  private var synthesizer: Option[Synthesizer] = None
  private var receiver: Option[Receiver] = None
  
  // Track the current instrument for each channel
  private val channelInstruments = mutable.Map[Int, MidiInstrument]()
  
  // MIDI constants
  private val DEFAULT_VELOCITY = 64 // Default velocity (volume)
  private val DEFAULT_CHANNEL = 0
  private val PERCUSSION_CHANNEL = 9
  private val MICROSECONDS_PER_MINUTE = 60000000L
  
  // Initialize MIDI
  private def initialize(): Unit = {
    if (synthesizer.isEmpty || receiver.isEmpty) {
      try {
        val synth = MidiSystem.getSynthesizer
        synth.open()
        val rcvr = synth.getReceiver
        
        // Initialize channels
        val channels = synth.getChannels
        
        // Set up default instruments for all channels
        channels.zipWithIndex.foreach { case (channel, i) =>
          if (i == PERCUSSION_CHANNEL) {
            // Channel 10 is reserved for percussion in GM
            channel.programChange(0)  // Bank select not needed for percussion
            channelInstruments(i) = MidiInstrument.Percussion
          } else {
            // Default to piano for other channels
            channel.programChange(0, 0)  // Bank 0, program 0 (Acoustic Grand Piano)
            channelInstruments(i) = MidiInstrument.Default
          }
        }
        
        synthesizer = Some(synth)
        receiver = Some(rcvr)
      } catch {
        case e: MidiUnavailableException =>
          System.err.println("MIDI device unavailable: " + e.getMessage)
      }
    }
  }
  
  // Convert volume (0.0 to 1.0) to MIDI velocity (0 to 127)
  private def volumeToVelocity(volume: Vol): Int = {
    math.min(127, math.max(0, (volume * 127).toInt))
  }
  
  // Convert note name to MIDI note number (C4 = 60)
  private def pitchToMidiNote(pitch: Pitch): Int = {
    val baseNote = pitch.pitchClass match {
      case PitchClass.C => 0
      case PitchClass.CSharp => 1
      case PitchClass.D => 2
      case PitchClass.DSharp => 3
      case PitchClass.E => 4
      case PitchClass.F => 5
      case PitchClass.FSharp => 6
      case PitchClass.G => 7
      case PitchClass.GSharp => 8
      case PitchClass.A => 9
      case PitchClass.ASharp => 10
      case PitchClass.B => 11
    }
    baseNote + (pitch.octave + 1) * 12
  }
  
  // Set instrument for a specific channel
  private def setInstrument(channel: Int, instrument: MidiInstrument): Unit = {
    (synthesizer, receiver) match {
      case (Some(synth), Some(rcvr)) =>
        // Only change if the instrument is different
        if (!channelInstruments.get(channel).contains(instrument)) {
          val channels = synth.getChannels
          
          // Bank select (MSB and LSB)
          val bankMsb = (instrument.bank >> 7) & 0x7F
          val bankLsb = instrument.bank & 0x7F
          
          // Send bank select messages
          val bankMsbMsg = new ShortMessage(ShortMessage.CONTROL_CHANGE, channel, 0, bankMsb)
          val bankLsbMsg = new ShortMessage(ShortMessage.CONTROL_CHANGE, channel, 32, bankLsb)
          
          rcvr.send(bankMsbMsg, -1)
          rcvr.send(bankLsbMsg, -1)
          
          // Program change
          val programChange = new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, instrument.program, 0)
          rcvr.send(programChange, -1)
          
          // Update the current instrument
          channelInstruments(channel) = instrument
        }
      case _ =>
        System.err.println("MIDI device not available")
    }
  }
  
  // Play a single note
  def playNote(
    pitch: Pitch, 
    duration: Long, 
    volume: Vol = 0.7, 
    instrument: MidiInstrument = MidiInstrument.Default
  ): Unit = {
    if (synthesizer.isEmpty || receiver.isEmpty) initialize()
    
    (synthesizer, receiver) match {
      case (Some(synth), Some(rcvr)) =>
        val channel = instrument.channel
        
        // Set the instrument if needed
        setInstrument(channel, instrument)
        
        val noteOn = new ShortMessage()
        noteOn.setMessage(ShortMessage.NOTE_ON, channel, pitchToMidiNote(pitch), volumeToVelocity(volume))
        rcvr.send(noteOn, -1)
        
        // Schedule note off
        val noteOff = new ShortMessage()
        noteOff.setMessage(ShortMessage.NOTE_OFF, channel, pitchToMidiNote(pitch), 0)
        rcvr.send(noteOff, synth.getMicrosecondPosition + duration)
        
      case _ =>
        System.err.println("MIDI device not available")
    }
  }
  
  // Play a sequence of notes
  def playSequence(notes: Seq[Note], tempo: Int = 120): Unit = {
    if (synthesizer.isEmpty || receiver.isEmpty) initialize()
    
    (synthesizer, receiver) match {
      case (Some(synth), Some(rcvr)) =>
        val bpm = tempo
        val microsecondsPerBeat = MICROSECONDS_PER_MINUTE / bpm
        
        // Calculate absolute time for each note
        var currentTime = synth.getMicrosecondPosition
        
        // Track active notes for each channel to handle overlaps
        val activeNotes = mutable.Map[Int, List[(Int, Long)]]()
        
        for (note <- notes) {
          val channel = note.instrument.channel
          
          // Set the instrument if needed
          setInstrument(channel, note.instrument)
          
          // Convert duration to microseconds
          val durationMs = (note.duration.value * 4 * 1000 * (60.0 / bpm)).toLong * 1000
          
          // Play the note
          val noteNumber = pitchToMidiNote(note.pitch)
          val velocity = volumeToVelocity(note.volume)
          
          // Send note on
          val noteOn = new ShortMessage(ShortMessage.NOTE_ON, channel, noteNumber, velocity)
          rcvr.send(noteOn, currentTime)
          
          // Schedule note off
          val noteOff = new ShortMessage(ShortMessage.NOTE_OFF, channel, noteNumber, 0)
          rcvr.send(noteOff, currentTime + durationMs)
          
          // Update active notes
          val noteEndTime = currentTime + durationMs
          val currentNotes = activeNotes.getOrElse(channel, List.empty[(Int, Long)])
          activeNotes(channel) = (noteNumber, noteEndTime) :: currentNotes
          
          // Move to next note position
          currentTime += durationMs
        }
        
        // Wait for the last note to finish
        val waitTime = (currentTime - synth.getMicrosecondPosition) / 1000
        if (waitTime > 0) {
          Thread.sleep(waitTime)
        }
        
      case _ =>
        System.err.println("MIDI device not available")
    }
  }
  
  // Play a score
  def playScore(score: Score): Unit = {
    if (synthesizer.isEmpty || receiver.isEmpty) initialize()
    
    for (measure <- score.measures) {
      val notes = measure.elements.collect { case n: Note => n }
      
      // Group notes by instrument to minimize program changes
      val notesByInstrument = notes.groupBy(_.instrument)
      
      // Play each instrument's notes in sequence
      notesByInstrument.foreach { case (instrument, instrumentNotes) =>
        playSequence(instrumentNotes, measure.tempo.bpm)
      }
    }
  }
  
  // Clean up resources
  def close(): Unit = {
    receiver.foreach(_.close())
    synthesizer.foreach(_.close())
    receiver = None
    synthesizer = None
  }
}

object MidiPlayer {
  def apply(): MidiPlayer = new MidiPlayer()
  
  // Play a score with a new MidiPlayer instance
  def play(score: Score): Unit = {
    val player = new MidiPlayer()
    try {
      player.playScore(score)
    } finally {
      player.close()
    }
  }
  
  // Play a sequence of notes with a new MidiPlayer instance
  def play(notes: Seq[Note], tempo: Int = 120): Unit = {
    val player = new MidiPlayer()
    try {
      player.playSequence(notes, tempo)
    } finally {
      player.close()
    }
  }
}
