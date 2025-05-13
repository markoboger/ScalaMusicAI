package de.htwg.scalamusic.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.scalamusic.model.DurationValue
import de.htwg.scalamusic.Duration

class DurationSpec extends AnyWordSpec with Matchers {
  // Small epsilon for floating point comparisons
  private val epsilon = 0.000001
  
  "DurationValue" when {
    import DurationValue._
    
    "created" should {
      "have correct values" in {
        (Whole.value - 1.0).abs should be < epsilon
        (Half.value - 0.5).abs should be < epsilon
        (Quarter.value - 0.25).abs should be < epsilon
        (Eighth.value - 0.125).abs should be < epsilon
        (Sixteenth.value - 0.0625).abs should be < epsilon
      }
      
      "have correct string representations" in {
        // Check the string representation of each duration
        Whole.toString shouldBe "Whole"
        Half.toString shouldBe "Half"
        Quarter.toString shouldBe "Quarter"
        Eighth.toString shouldBe "Eighth"
        Sixteenth.toString shouldBe "Sixteenth"
      }
    }
    
    "multiplied by a factor" should {
      "return a new duration with correct value" in {
        val half = Half * 0.5
        (half.value - 0.25).abs should be < epsilon
        
        // Multiplying by 1.0 returns the same duration type
        val quarter = Quarter * 1.0
        quarter shouldBe Quarter
      }
      
      "handle zero and negative factors" in {
        // Zero duration - should return a CustomDuration with value 0.0
        val zero = Quarter * 0.0
        zero.value shouldBe 0.0
        
        // Negative factor (sign is preserved in the implementation)
        val negative = Quarter * (-2.0)
        negative.value shouldBe -0.5  // 0.25 * -2.0
        
        // Test with factor of 1.0 (should return the original duration)
        val same = Quarter * 1.0
        same shouldBe Quarter
      }
    }
    
    "dotted" should {
      "extend the duration by 50%" in {
        (Quarter.dotted.value - 0.375).abs should be < epsilon  // 0.25 * 1.5
        (Eighth.dotted.value - 0.1875).abs should be < epsilon  // 0.125 * 1.5
      }
      
      "be chainable" in {
        val doubleDotted = Quarter.dotted.dotted
        (doubleDotted.value - 0.5625).abs should be < epsilon  // 0.25 * 1.5 * 1.5
      }
    }
  }
  
  "DurationMultiplier" should {
    "have correct values for common tuplets" in {
      // Test that we can create a triplet duration (2/3 of the original)
      val triplet = DurationValue.Quarter * (2.0 / 3.0)
      (triplet.value - (0.25 * (2.0 / 3.0))).abs should be < epsilon
    }
    
    "be applicable to duration values" in {
      // Test multiplication with custom factors
      val halfDuration = DurationValue.Half * 0.5
      (halfDuration.value - 0.25).abs should be < epsilon
      
      val dottedEighth = DurationValue.Eighth * 1.5
      (dottedEighth.value - (0.125 * 1.5)).abs should be < epsilon
    }
    
    "handle dotted durations correctly" in {
      // Test the dotted method
      val dottedQuarter = DurationValue.Quarter.dotted
      (dottedQuarter.value - (0.25 * 1.5)).abs should be < epsilon
      
      val doubleDottedHalf = DurationValue.Half.dotted.dotted
      (doubleDottedHalf.value - (0.5 * 1.5 * 1.5)).abs should be < epsilon
    }
  }
}
