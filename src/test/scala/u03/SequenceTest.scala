package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*

class SequenceTest:

  import u03.Sequences.*
  import Sequence.Cons
  import Sequence.Nil

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  import Sequence.sum

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  import Sequence.map

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  import Sequence.filter

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  import exercise.Exercise.Sequences.Sequence.*

  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(l2, Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Just(10), min(Cons(25, Cons(20, Cons(10, Nil())))))
    assertEquals(Empty(), min(Nil()))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    val emptyList: Sequence[Int] = Nil()
    assertEquals(0, foldLeft(emptyList)(0)(_ + _))

  import u02.Modules.Person

  val persons: Sequence[Person] = Cons(Person.Teacher("t1", "c1"), Cons(Person.Student("s1", 2000), Cons(Person.Teacher("t2", "c2"), Nil())))

  @Test def testGetCourses() =
    assertEquals(Cons("c1", Cons("c2", Nil())), getCourses(persons))
    assertEquals(Nil(), getCourses(Nil()))
