package exercise

import u02.Modules.Person
import u03.Optionals.Optional

import scala.annotation.tailrec

object Exercise {

  object Sequences:

    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.Cons
    import u03.Sequences.Sequence.Nil

    object Sequence:

      // Task 01
      def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _ => Nil()

      def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
        case _ => Nil()

      def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
        case Cons(h, t) => Cons(h, concat(t, l2))
        case _ => l2

      def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case _ => Nil()

      def min(l: Sequence[Int]): Optional[Int] =
        @tailrec
        def _min(l: Sequence[Int], currentMin: Int = Int.MaxValue): Int = l match
          case Cons(h, t) => _min(t, if h < currentMin then h else currentMin)
          case _ => currentMin

        l match
          case Cons(_, _) => Optional.Just(_min(l))
          case _ => Optional.Empty()

      // Task 02
      def getCourses(l: Sequence[Person]): Sequence[String] = flatMap(l)({
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      })

      @tailrec
      def foldLeft[A, B](l: Sequence[A])(default: B)(op: (B, A) => B): B = l match
        case Cons(h, t) => foldLeft(t)(op(default, h))(op)
        case _ => default

      extension [A](s: Sequence[A])

        def zipExt[B](other: Sequence[B]): Sequence[(A, B)] = (s, other) match
          case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zipExt(t2))
          case _ => Nil()

        def takeExt(n: Int): Sequence[A] = s match
          case Cons(h, t) if n > 0 => Cons(h, t.takeExt(n - 1))
          case _ => Nil()

        def concatExt(other: Sequence[A]): Sequence[A] = s match
          case Cons(h, t) => Cons(h, t.concatExt(other))
          case _ => other

        def flatMapExt[B](mapper: A => Sequence[B]): Sequence[B] = s match
          case Cons(h, t) => mapper(h).concatExt(t.flatMapExt(mapper))
          case _ => Nil()

        @tailrec
        def foldLeftExt[B](default: B)(op: (B, A) => B): B = s match
          case Cons(h, t) => t.foldLeftExt(op(default, h))(op)
          case _ => default

      extension (s: Sequence[Int])

        def minExt(): Optional[Int] =
          extension (s: Sequence[Int])

            @tailrec
            def _min(currentMin: Int = Int.MaxValue): Int = s match
              case Cons(h, t) => s._min(if h < currentMin then h else currentMin)
              case _ => currentMin

          s match
            case Cons(_, _) => Optional.Just(s._min())
            case _ => Optional.Empty()

      extension (s: Sequence[Person])

        def getCoursesExt: Sequence[String] = flatMap(s)({
          case Person.Teacher(_, c) => Cons(c, Nil())
          case _ => Nil()
        })

  object Streams:

    import u03.Sequences.*

    enum Stream[A]:
      private case Empty()
      private case Cons(head: () => A, tail: () => Stream[A])

    object Stream:

      def empty[A](): Stream[A] = Empty()

      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

      def toList[A](stream: Stream[A]): Sequence[A] = stream match
        case Cons(h, t) => Sequence.Cons(h(), toList(t()))
        case _ => Sequence.Nil()

      def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
        case Cons(head, tail) => cons(f(head()), map(tail())(f))
        case _ => Empty()

      def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
        case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
        case Cons(head, tail) => filter(tail())(pred)
        case _ => Empty()

      def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
        case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
        case _ => Empty()

      def iterate[A](init: => A)(next: A => A): Stream[A] =
        cons(init, iterate(next(init))(next))

      // Task 03
      def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
        case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
        case _ => Empty()

      def fill[A](n: Int)(k: A): Stream[A] = n match
        case a if a <= 0 => Empty()
        case _ => cons(k, fill(n - 1)(k))

      def pell(): Stream[Int] =
        var n2 = 0

        def _pell(head: Int): Stream[Int] =
          lazy val tail = () => _pell(head match
            case 0 => 1
            case n1 =>
              val next = 2 * n1 + n2
              n2 = n1
              next
          )
          Cons(() => head, tail)

        _pell(0)
}
