package de.htwg.scalamusic.dsl

import de.htwg.scalamusic.model._
import de.htwg.scalamusic.model.Instrument._
import scala.language.higherKinds
import scala.language.postfixOps

/**
 * Type class for monadic music operations
 */
trait MusicMonad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  
  // Derived operations
  def sequence[A](xs: List[M[A]]): M[List[A]] = 
    xs.foldRight(pure(List.empty[A])) { (a, acc) =>
      flatMap(a)(aa => map(acc)(aa :: _))
    }
    
  def traverse[A, B](xs: List[A])(f: A => M[B]): M[List[B]] =
    sequence(xs.map(f))
}

object MusicMonad {
  def apply[M[_]](implicit ev: MusicMonad[M]): MusicMonad[M] = ev
  
  // Syntax for monadic operations
  implicit class MusicMonadOps[M[_], A](ma: M[A])(implicit M: MusicMonad[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.flatMap(ma)(f)
    def map[B](f: A => B): M[B] = M.map(ma)(f)
    def >>[B](mb: M[B]): M[B] = M.flatMap(ma)(_ => mb)
  }
}

/**
 * MusicT monad transformer for composing musical operations
 */
case class MusicT[F[_], A](run: F[MusicContext => F[(MusicContext, A)]])(implicit F: MusicMonad[F]) {
  def flatMap[B](f: A => MusicT[F, B]): MusicT[F, B] =
    MusicT(F.flatMap(run) { g =>
      F.pure { (ctx: MusicContext) =>
        F.flatMap(g(ctx)) { case (newCtx, a) =>
          val mt = f(a)
          F.flatMap(mt.run) { f2 =>
            f2(newCtx)
          }
        }
      }
    })
    
  def map[B](f: A => B): MusicT[F, B] =
    MusicT(F.map(run) { g => (ctx: MusicContext) =>
      F.map(g(ctx)) { case (newCtx, a) => (newCtx, f(a)) }
    })
    
  def runWith(initialCtx: MusicContext): F[(MusicContext, A)] = {
    F.flatMap(run) { f =>
      f(initialCtx)
    }
  }
  
  def flatMapF[B](f: A => F[B]): MusicT[F, B] =
    flatMap(a => MusicT.liftF(f(a)))
}

object MusicT {
  // Monad instance for MusicT
  implicit def musicTMonad[F[_]](implicit F0: MusicMonad[F]): MusicMonad[({ type λ[α] = MusicT[F, α] })#λ] = 
    new MusicMonad[({ type λ[α] = MusicT[F, α] })#λ] {
      def pure[A](a: A): MusicT[F, A] = 
        MusicT(F0.pure((ctx: MusicContext) => F0.pure((ctx, a))))
      
      def flatMap[A, B](ma: MusicT[F, A])(f: A => MusicT[F, B]): MusicT[F, B] =
        ma.flatMap(f)
      
      override def map[A, B](ma: MusicT[F, A])(f: A => B): MusicT[F, B] =
        ma.map(f)
    }
  
  // Helper methods
  def getContext[F[_]: MusicMonad]: MusicT[F, MusicContext] = {
    val F = implicitly[MusicMonad[F]]
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((ctx, ctx))
    })
  }
    
  def modifyContext[F[_]: MusicMonad](f: MusicContext => MusicContext): MusicT[F, Unit] = {
    val F = implicitly[MusicMonad[F]]
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((f(ctx), ()))
    })
  }
    
  def liftF[F[_]: MusicMonad, A](fa: F[A]): MusicT[F, A] = {
    val F = implicitly[MusicMonad[F]]
    MusicT(F.map(fa) { a => (ctx: MusicContext) =>
      F.pure((ctx, a))
    })
  }
}

/**
 * Music context that can be modified through monadic operations
 */
case class MusicContext(
  tempo: Int = 120,
  instrument: MidiInstrument = MidiInstrument.Default,
  volume: Double = 0.7,
  timeSignature: TimeSignature = TimeSignature(4, 4),
  position: Int = 0,  // Current position in ticks
  key: Key = Key.CMajor
) {
  def withTempo(newTempo: Int): MusicContext = copy(tempo = newTempo)
  def withInstrument(newInstrument: MidiInstrument): MusicContext = copy(instrument = newInstrument)
  def withVolume(newVolume: Double): MusicContext = copy(volume = newVolume)
  def withTimeSignature(newTimeSignature: TimeSignature): MusicContext = copy(timeSignature = newTimeSignature)
  def withKey(newKey: Key): MusicContext = copy(key = newKey)
  def advancePosition(ticks: Int): MusicContext = copy(position = position + ticks)
}

/**
 * Key signature representation
 */
sealed trait Key
object Key {
  case object CMajor extends Key
  case object GMajor extends Key
  case object DMajor extends Key
  case object AMajor extends Key
  case object EMajor extends Key
  case object BMajor extends Key
  case object FSharpMajor extends Key
  case object CSharpMajor extends Key
  case object FMajor extends Key
  case object BFlatMajor extends Key
  case object EFlatMajor extends Key
  case object AFlatMajor extends Key
  case object DFlatMajor extends Key
  case object GFlatMajor extends Key
  case object CFlatMajor extends Key
  
  // Minor keys
  case object AMinor extends Key
  case object EMinor extends Key
  case object BMinor extends Key
  case object FSharpMinor extends Key
  case object CSharpMinor extends Key
  case object GSharpMinor extends Key
  case object DSharpMinor extends Key
  case object ASharpMinor extends Key
  case object DMinor extends Key
  case object GMinor extends Key
  case object CMinor extends Key
  case object FMinor extends Key
  case object BFlatMinor extends Key
  case object EFlatMinor extends Key
  case object AFlatMinor extends Key
}

/**
 * Music builder DSL that uses monadic operations
 */
class MusicBuilder[F[_]](implicit val F: MusicMonad[F]) {
  import MusicMonad._
  
  // Constants for tick calculations
  private val TicksPerQuarterNote = 480  // Standard MIDI ticks per quarter note
  
  // Calculate the number of ticks for a given duration and tempo
  private def calculateTicks(duration: DurationValue, tempo: Int): Int = {
    // Convert duration in beats to ticks
    // For example, at 120 BPM, a quarter note is 500ms (60000/120)
    // So at 480 ticks/quarter note, each tick is 500/480 ≈ 1.04ms
    // For a given duration (as a fraction of a whole note), the ticks are:
    // ticks = duration.value * (ticksPerQuarterNote * 4)  // 4 quarter notes in a whole note
    (duration.value * TicksPerQuarterNote * 4).toInt
  }
  
  // Basic operations
  def playNote(pitch: Pitch, duration: DurationValue): MusicT[F, Unit] = 
    MusicT(F.pure { (ctx: MusicContext) =>
      val ticks = calculateTicks(duration, ctx.tempo)
      F.pure((ctx.advancePosition(ticks), ()))
    })
    
  /**
   * Play a note with a default quarter note duration
   * @param pitch The pitch to play
   * @param duration Optional duration (defaults to quarter note)
   * @return MusicT monad representing the note
   */
  def play(pitch: Pitch, duration: DurationValue = DurationValue.Quarter): MusicT[F, Unit] =
    playNote(pitch, duration)
    
  def rest(duration: DurationValue): MusicT[F, Unit] =
    MusicT(F.pure { (ctx: MusicContext) =>
      val ticks = calculateTicks(duration, ctx.tempo)
      F.pure((ctx.advancePosition(ticks), ()))
    })
    
  // Context manipulation
  def withTempo(tempo: Int): MusicT[F, Unit] = 
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((ctx.withTempo(tempo), ()))
    })
    
  def withInstrument(instrument: MidiInstrument): MusicT[F, Unit] =
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((ctx.withInstrument(instrument), ()))
    })
    
  def withVolume(volume: Double): MusicT[F, Unit] =
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((ctx.withVolume(volume), ()))
    })
    
  def withTimeSignature(timeSignature: TimeSignature): MusicT[F, Unit] =
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((ctx.withTimeSignature(timeSignature), ()))
    })
    
  def withKey(key: Key): MusicT[F, Unit] =
    MusicT(F.pure { (ctx: MusicContext) =>
      F.pure((ctx.withKey(key), ()))
    })
    
  // Composition operators
  def sequence(actions: MusicT[F, Unit]*): MusicT[F, Unit] = 
    actions.foldLeft(MusicT(F.pure((ctx: MusicContext) => F.pure((ctx, ()))))) {
      (acc, action) => acc.flatMap(_ => action)
    }
    
  def parallel(actions: MusicT[F, Unit]*): MusicT[F, Unit] = {
    // In a real implementation, this would play all actions simultaneously
    // For simplicity, we'll just sequence them for now
    sequence(actions: _*)
  }
  
  // Helper method to run a music program
  def runMusic(program: MusicT[F, Unit], initialCtx: MusicContext = MusicContext()): F[(MusicContext, Unit)] = 
    program.runWith(initialCtx)
}

object MusicBuilder {
  def apply[F[_]: MusicMonad]: MusicBuilder[F] = new MusicBuilder[F]
  
  // Example usage
  def example[F[_]: MusicMonad]: MusicT[F, Unit] = {
    val builder = MusicBuilder[F]
    import builder._
    
    for {
      _ <- withTempo(120)
      _ <- withInstrument(MidiInstrument.Piano)
      _ <- sequence(
        playNote(Pitch.C(4), DurationValue.Quarter),
        playNote(Pitch.E(4), DurationValue.Quarter),
        playNote(Pitch.G(4), DurationValue.Quarter),
        playNote(Pitch.C(5), DurationValue.Quarter)
      )
    } yield ()
  }
}

/**
 * Identity monad for MusicT
 */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  implicit val idMonad: MusicMonad[Id] = new MusicMonad[Id] {
    def pure[A](a: A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }
}

/**
 * Example of using the monadic music DSL
 */
object MusicMonadExample extends App {
  // Create a simple music program
  val program = for {
    _ <- MusicBuilder[Id].withTempo(120)
    _ <- MusicBuilder[Id].withInstrument(MidiInstrument.Piano)
    _ <- MusicBuilder[Id].playNote(Pitch.C(4), DurationValue.Quarter)
    _ <- MusicBuilder[Id].playNote(Pitch.E(4), DurationValue.Quarter)
    _ <- MusicBuilder[Id].playNote(Pitch.G(4), DurationValue.Quarter)
    _ <- MusicBuilder[Id].playNote(Pitch.C(5), DurationValue.Quarter)
  } yield ()
  
  // Run the program with initial context
  val initialContext = MusicContext()
  val builder = new MusicBuilder[Id]()
  val result = builder.runMusic(program, initialContext).value
  val (finalContext, _) = result
  
  println(s"Initial context: $initialContext")
  println(s"Final context: $finalContext")
  
  // Print some statistics
  println(s"Final position: ${finalContext.position} ticks")
  println(s"Final tempo: ${finalContext.tempo} BPM")
  println(s"Final instrument: ${finalContext.instrument}")
}
