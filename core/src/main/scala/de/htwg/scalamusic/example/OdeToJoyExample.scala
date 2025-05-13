package de.htwg.scalamusic.example

import de.htwg.scalamusic.dsl.MusicDSL
import de.htwg.scalamusic.model._


/**
 * Example of using the MusicDSL to play "Ode to Joy" by Beethoven
 */
object OdeToJoyExample extends App with MusicDSL {
  // Import duration values for easier access
  import de.htwg.scalamusic.dsl.MusicDSL._
  
  // Define the melody (simplified version of Ode to Joy)
  val melody = Seq(
    E(4) * q,
    E(4) * q,
    F(4) * q,
    G(4) * q,
    G(4) * q,
    F(4) * q,
    E(4) * q,
    D(4) * q,
    C(4) * q,
    C(4) * q,
    D(4) * q,
    E(4) * q,
    E(4) * (q, DurationMultiplier.Triplet),
    D(4) * (e, DurationMultiplier.Triplet),
    D(4) * h
  )
  
  // Create measures with the melody
  val measure1 = measure(
    Note(E(4), q),
    Note(E(4), q),
    Note(F(4), q),
    Note(G(4), q)
  )
  
  val measure2 = measure(
    Note(G(4), q),
    Note(F(4), q),
    Note(E(4), q),
    Note(D(4), q)
  )
  
  val measure3 = measure(
    Note(C(4), q),
    Note(C(4), q),
    Note(D(4), q),
    Note(E(4), q)
  )
  
  val measure4 = measure(
    Note(E(4), q * DurationMultiplier.Triplet.factor),
    Note(D(4), e * DurationMultiplier.Triplet.factor),
    Note(D(4), h)
  )
  
  // Create a score with the measures
  val odeToJoyScore = score(
    measure1,
    measure2,
    measure3,
    measure4
  )
  
  // Print the score
  println("Ode to Joy (simplified):")
  odeToJoyScore.measures.zipWithIndex.foreach { case (m, i) =>
    println(s"\nMeasure ${i + 1} (${m.timeSignature.numerator}/${m.timeSignature.denominator}):")
    m.elements.foreach {
      case Note(pitch, duration, _, _, _) => 
        println(s"  $pitch (${duration.value})")
      case rest => println(s"  $rest")
    }
  }
  
  // Play the score
  println("\nPlaying Ode to Joy...")
  play(odeToJoyScore)
  
  // Wait a bit between plays
  Thread.sleep(1000)
  
  // Play just the melody with a different tempo
  println("\nPlaying just the melody faster...")
  play(melody.map(p => Note(p.pitch, p.duration)), 180)  // Faster tempo (180 BPM)
}
