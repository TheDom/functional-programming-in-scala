package com.dominikgruber.fpinscala.chapter09

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err,Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice

    /**
     * Exercise 09 (hard)
     * At this point, you are going to take over the process. You will be
     * creating a Parser[JSON] from scratch using the primitives we have
     * defined. You don't need to worry (yet) about the representation of
     * Parser. As you go, you will undoubtedly discover additional combinators
     * and idioms, notice and factor out common patterns, and so on.
     */

    // Unfinished attempt
    /*
    val jNull = string("null").map(_ => JNull)
    val jNumber = "\\d+(\\.\\d*)?".r.map(s => JNumber(s.toDouble))
    val jString = "\".*\"".r.map(s => JString(s))
    val jBool = "((true)|(false))".r.map(s => JBool(s.toBoolean))

    val jArrayContent = jValue.flatMap(s => product(succeed(s), (char(',') ** jValue).map(t => t._2).many)).map(l => (l._1 :: l._2).toIndexedSeq)
    val jArray = (char('[') ** jArrayContent ** char(']')).map(s => JArray(s._1._2))

    val jObjectEntry = (jString ** char(':') ** jValue).map(s => (s._1._1.get, s._2))
    val jObjectContent = jObjectEntry.flatMap(e => product(succeed(e), (char(',') ** jObjectEntry).map(t => t._2).many)).map(l => (l._1 :: l._2).toMap)
    val jObject = (char('{') ** jObjectContent ** char('}')).map(s => JObject(s._1._2))

    def jValue: Parser[JSON] = jNull | jNumber | jString | jBool | jArray | jObject

    jArray | jObject
    */

    // Keep compiler happy
    succeed(JNull)
  }
}