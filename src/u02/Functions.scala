package u02

import scala.util.Random

import scala.annotation.tailrec;

object Functions extends App:
  // Tasks
  // Part 2
  // Part 2.a
  private def posMeth(x: Int): String = x match
    case n if n > 0 => "positive"
    case _ => "negative"
  private val posLambda: Int => String = { case x if x >= 0 => "positive" case _ => "negative"}

  println("Tasks:")
  val prefix: Int => String = t => "\t" * t

  println(s"${prefix(1)} Part 2:")
  println(s"${prefix(2)} Part 2.a:")
  println(s"${prefix(3)} Positive with method syntax: ${posMeth(1)}, expected: positive")
  println(s"${prefix(3)} Positive with lambda syntax: ${posLambda(-2)}, expected: negative")

  def negMeth(f: String => Boolean): String => Boolean = x => !f(x)
  val negLambda: (String => Boolean) => String => Boolean = f => s => !f(s)

  val empty: String => Boolean = _ == ""
  val s1 = "foo"
  val s2 = ""
  val notEmptyLambda = negLambda(empty)
  println(s"${prefix(3)} Predicate as lambda: (${notEmptyLambda(s1)}, ${notEmptyLambda(s2)}), " +
    s"expected: (${true}, ${false})")

  val notEmptyMeth = negMeth(empty)
  println(s"${prefix(3)} Predicate as method: (${notEmptyMeth(s1)}, ${notEmptyMeth(s2)}), " +
    s"expected: (${true}, ${false})")

  def neg[A](f: A => Boolean): A => Boolean = x => !f(x)
  val isZero: Int => Boolean = _ == 0
  val isNotZero = neg(isZero)

  println(s"${prefix(3)} Generic predicate: ${isZero(1)}, expected: ${false}")

  // Part 2.b
  println(s"${prefix(2)} Part 2.b:")

  val notCurrF: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  println(s"${prefix(3)} Not curryied function: ${notCurrF(10, 20, 20)}, expected: ${true}")

  val currF: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  println(s"${prefix(3)} Curryied function: ${currF(10)(20)(21)}, , expected: ${false}")

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  println(s"${prefix(3)} Function composition: ${compose(_ - 1, _ * 2)(5)}, expected: 9")

  // Generic composition: g's co-domine must be equal to f's domine
  def genCompose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))
  val not: Boolean => Boolean = b => !b

  val randInc: Int => Double = x => x + Random.nextDouble()
  val square: Int => Int = x => x * x
  println(s"${prefix(3)} Function composition as generic function. Not: " +
    s"${genCompose(not, not)(true)}, expected: ${true}. Random increase: ${genCompose(randInc, square)(2)}")

  // Part 3
  println(s"${prefix(2)} Part 3:")

  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match
    case _ if b == 0 => a
    case _ => gcd(b, a % b)
  println(s"${prefix(3)} GCD as tail recursive function: (${gcd(8, 12)}, ${gcd(14, 7)}), expected: (4, 7)")

  // Part 4
  println(s"${prefix(2)} Part 4:")

  enum Shape:
    case Rectangle(center: (Double, Double), base: Double, height: Double)
    case Square(center: (Double, Double), side: Double)
    case Circle(center: (Double, Double), radius: Double)

  object Shape:

    def perimeter(shape: Shape): Double = shape match
      case Shape.Rectangle(_, base, height) => base * 2 + height * 2
      case Shape.Square(_, side) => side * 4
      case Shape.Circle(_, radius) => 2 * Math.PI * radius

    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Shape.Rectangle(center, base, height) =>
        (point._1 <= center._1 + base / 2 && point._1 >= center._1 - base / 2) &&
          (point._2 <= center._2 + height / 2 && point._2 >= center._2 - height / 2)

      case Shape.Square(center, side) =>
        (point._1 <= center._1 + side / 2 && point._1 >= center._1 - side / 2) &&
          (point._2 <= center._2 + side / 2 && point._2 >= center._2 - side / 2)

      case Shape.Circle(center, radius) =>
        Math.pow(point._1 - center._1, 2) + Math.pow(point._2 - center._2, 2) < Math.pow(radius, 2)

  import Shape.*;

  println(s"${prefix(3)} 'Test' perimeter:")

  val pRect: (Double, Double, Double) => String = (b, h, p) =>
    s"${prefix(4)} Rectangle perimeter with base = $b, height = $h: " +
    s"computed perimeter = ${perimeter(Rectangle((0, 0), b, h))}, expected = $p"
  println(pRect(4, 3, 14))

  val pSqr: (Double, Double) => String = (s, p) => s"${prefix(4)} Square perimeter with side = $s: " +
    s"computed perimeter = ${perimeter(Square((0, 0), s))}, expected = $p"
  println(pSqr(3, 12))

  val pCirc: (Double, Double) => String = (r, p) => s"${prefix(4)} Circle perimeter with radius = $r: " +
    s"computed circumference = ${perimeter(Circle((0, 0), r))}, expected = $p"
  println(pCirc(2, 4 * Math.PI))

  println(s"${prefix(3)} 'Test' contains:")

  val cRect: (Rectangle, (Double, Double), Boolean) => String =
    (r, p, b) => s"${prefix(4)} Rectangle with center = ${r.center}, base = ${r.base}, height = ${r.height} contains " +
    s"point $p: computed: ${contains(r, p)}, expected: $b"

  println(cRect(Rectangle((0, 0), 4, 3), (1, 0), true))
  println(cRect(Rectangle((0, 0), 4, 3), (4, 0), false))

  val cSqr: (Square, (Double, Double), Boolean) => String =
    (s, p, b) => s"${prefix(4)} Square with center = ${s.center}, side = ${s.side} contains " +
      s"point $p: computed: ${contains(s, p)}, expected: $b"

  println(cSqr(Square((0, 0), 3), (1, 0), true))
  println(cSqr(Square((0, 0), 3), (2, 0), false))

  val cCirc: (Circle, (Double, Double), Boolean) => String =
    (c, p, b) => s"${prefix(4)} Circle with center = ${c.center}, radius = ${c.radius} contains " +
      s"point $p: computed: ${contains(c, p)}, expected: $b"

  println(cCirc(Circle((0, 0), 3), (1, 1), true))
  println(cCirc(Circle((0, 0), 3), (4, 1), false))

  // Part 5
  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = opt match
      case Some(a) => f(a) match
        case true => Some(a)
        case false => None()
      case _ => None()

    def map[A](opt: Option[A])(f: A => Boolean): Option[Boolean] = opt match
      case Some(a) => f(a) match
        case true => Some(true)
        case false => Some(false)
      case _ => None()

    def fold(opt: Option[Int])(v: Int)(f: Int => Int): Int = opt match
      case Some(a) => f(a)
      case _ => v

  import Option.*

  println(s"${prefix(2)} Part 2")
  println(s"${prefix(3)} Filter:")

  println(s"${prefix(4)} (1): ${filter(Some(5))(_ > 2)}, expected: ${Some(5)}")
  println(s"${prefix(4)} (2): ${filter(Some(5))(_ > 8)}, expected: ${None()}")
  println(s"${prefix(4)} (3): ${filter(None[Int]())(_ > 2)}, expected: ${None()}")

  println(s"${prefix(3)} Map:")

  println(s"${prefix(4)} (1): ${map(Some(5))(_ > 2)}, expected: ${Some(true)}")
  println(s"${prefix(4)} (2): ${map(Some(5))(_ > 8)}, expected: ${Some(false)}")
  println(s"${prefix(4)} (3): ${map(None[Int]())(_ > 2)}, expected: ${None()}")

  println(s"${prefix(3)} 'Fold:")
  println(s"${prefix(4)} (1): ${fold(Some(5))(1)(_ + 1)}, expected: 6")
  println(s"${prefix(4)} (2): ${fold(None[Int]())(1)(_ + 1)}, expected: 1")




