package de.htwg.scalamusic.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class InstrumentSpec extends AnyWordSpec with Matchers {
  "MidiInstrument" when {
    "created" should {
      "have correct program and bank values" in {
        // The actual implementation uses default values for bank (0) and doesn't have named instruments
        // We'll test the default constructor behavior instead
        val piano = MidiInstrument(0)
        piano.program shouldBe 0
        piano.bank shouldBe 0
        
        val guitar = MidiInstrument(24)
        guitar.program shouldBe 24
        guitar.bank shouldBe 0
        
        val violin = MidiInstrument(40)
        violin.program shouldBe 40
        violin.bank shouldBe 0
        
        val trumpet = MidiInstrument(56)
        trumpet.program shouldBe 56
        trumpet.bank shouldBe 0
        
        val percussion = MidiInstrument(0, 9, 0, percussion = true)
        percussion.program shouldBe 0
        percussion.channel shouldBe 9
        percussion.percussion shouldBe true
      }
      
      "have correct string representation" in {
        // The actual implementation just shows the raw values
        val piano = MidiInstrument(0)
        piano.toString shouldBe "MidiInstrument(0,0,0,false)"
        
        val guitar = MidiInstrument(24)
        guitar.toString shouldBe "MidiInstrument(24,0,0,false)"
      }
      
      "allow custom bank selection" in {
        val customPiano = MidiInstrument(0, 0, 1)
        customPiano.program shouldBe 0
        customPiano.bank shouldBe 1
      }
    }
    
    "compared" should {
      "be equal with same program, channel, bank and percussion flag" in {
        val inst1 = MidiInstrument(0, 1, 2, false)
        val inst2 = MidiInstrument(0, 1, 2, false)
        inst1 shouldEqual inst2
      }
      
      "not be equal with different attributes" in {
        val base = MidiInstrument(0, 1, 2, false)
        
        // Different program
        MidiInstrument(1, 1, 2, false) should not equal base
        
        // Different channel
        MidiInstrument(0, 2, 2, false) should not equal base
        
        // Different bank
        MidiInstrument(0, 1, 3, false) should not equal base
        
        // Different percussion flag
        MidiInstrument(0, 1, 2, true) should not equal base
      }
    }
  }
  
  "Instrument creation" should {
    "support all General MIDI instrument programs" in {
      // Test creating instruments across the GM range
      // Piano
      val piano = MidiInstrument(0)
      piano.program shouldBe 0
      
      // Guitar
      val guitar = MidiInstrument(24)
      guitar.program shouldBe 24
      
      // Strings
      val violin = MidiInstrument(40)
      violin.program shouldBe 40
      
      // Brass
      val trumpet = MidiInstrument(56)
      trumpet.program shouldBe 56
      
      // Percussion (channel 9, program 0, bank 0, percussion = true)
      val percussion = MidiInstrument(0, 9, 0, percussion = true)
      percussion.program shouldBe 0
      percussion.channel shouldBe 9
      percussion.bank shouldBe 0
      percussion.percussion shouldBe true
    }
  }
}
