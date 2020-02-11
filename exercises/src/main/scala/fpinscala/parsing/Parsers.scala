package fpinscala.parsing

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(x => succeed(f(x)))
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = p.flatMap(a => p2.map(a -> _))
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

//  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = product(p, p2).map(f.tupled)
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for (a <- p; b <- p2) yield f(a, b)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] = {
    def loop(r: Parser[List[A]]): Parser[List[A]] = loop(map2(p, r)(_ :: _)) | r
    loop(succeed(List.empty[A]))
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @scala.annotation.tailrec
    def loop(c: Int, r: Parser[List[A]]): Parser[List[A]] = if (c < n) loop(c + 1, map2(p, r)(_ :: _)) else r
    loop(n, succeed(List.empty[A]))
  }

  case class ParserOps[A](p: Parser[A]) {

    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
  }

  def numOfChars(c: Char): Parser[List[Char]] = "\\d+".r.flatMap(d => listOfN(d.toInt, char(c)))
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}