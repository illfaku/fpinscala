package fpinscala.parsing

trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def parser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val spaces: Parser[String] = "\\s*".r

    val jNull: Parser[JSON] = string("null").map(_ => JNull)

    val jNumber: Parser[JSON] = "-?\\d+(\\.\\d+)?(e\\d+)?".r.map(x => JNumber(x.toDouble))

    val stringWoQuotes: Parser[String] = "[^\"]*".r
    def stringOutOfQuotes: Parser[String] = {
      def loop(parts: List[String]): Parser[String] = {
        stringWoQuotes.flatMap { s =>
          if (s.endsWith("\\")) string("\"").flatMap(q => loop(q :: s :: parts))
          else succeed((s :: parts).reverse.mkString)
        }
      }
      for {
        _ <- char('"')
        s <- loop(Nil)
        _ <- char('"')
      } yield s
    }

    def jString: Parser[JSON] = stringOutOfQuotes.map(JString)

    val jBool: Parser[JSON] = ("true" | "false").map(x => JBool(x.toBoolean))

    def jArray: Parser[JSON] = {
      char('[').flatMap { _ =>
        def loop(vs: List[JSON]): Parser[IndexedSeq[JSON]] = {
          jValue.flatMap { v =>
            (char(']') | char(',')).flatMap {
              case ',' => loop(v :: vs)
              case _ => succeed((v :: vs).reverse.toIndexedSeq)
            }
          }
        }
        char(']').map(_ => JArray(IndexedSeq.empty)) | loop(Nil).map(JArray)
      }
    }

    def jField: Parser[(String, JSON)] = {
      for {
        _ <- spaces
        n <- stringOutOfQuotes
        _ <- spaces
        _ <- char(':')
        v <- jValue
      } yield n -> v
    }

    def jObject: Parser[JSON] = {
      char('{').flatMap { _ =>
        def loop(fs: Map[String, JSON]): Parser[Map[String, JSON]] = {
          jField.flatMap { f =>
            (char('}') | char(',')).flatMap {
              case ',' => loop(fs + f)
              case _ => succeed(fs + f)
            }
          }
        }
        char('}').map(_ => JObject(Map.empty)) | loop(Map.empty).map(JObject)
      }
    }

    def jValue: Parser[JSON] = for {
      _ <- spaces
      v <- jNull | jNumber | jString | jBool | jArray | jObject
      _ <- spaces
    } yield v

    jValue
  }

  def main(args: Array[String]): Unit = {
    println("true".toBoolean)
  }
}
