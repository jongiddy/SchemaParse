package org.catalogueoflife.e2.schema.parse

import scala.util.parsing.combinator.RegexParsers

class SchemaParser(er: ERGraph) extends RegexParsers {

	override val whiteSpace = """((//.*)?\s+)+""".r

	// The identifiers should be constrained to work well with database field labels and XML tags
	// XML is very flexible, but databases may not be
	lazy val unqualifiedName = "[A-Za-z_][A-Za-z0-9_]*".r
	lazy val qualifiedName = "[A-Za-z][A-Za-z0-9_]*([.][A-Za-z][A-Za-z0-9_]*)*".r
	// label contains singular and plural "name/names" or "name(s)"
	lazy val label: Parser[Names] = unqualifiedName ~ ((("/" ~ unqualifiedName) | ("(" ~> unqualifiedName <~ ")"))?) ^^ {
	    case (sing ~ None) => Names(new Name(sing), None)
	    case (sing ~ Some("/" ~ plural)) => Names(new Name(sing), Some(new Name(plural.toString))) // plural has type Any
	    case (sing ~ Some(add)) => Names(new Name(sing), Some(new Name(sing + add)))
	 }
	
	lazy val entityClause = ("{" ~> repsep(property, ",") <~ "}")
	
	lazy val property: Parser[Property] = ((description*) ~ (modifier?) ~ label ~ ":" ~ typeval) ^^ {
		case (docList ~ None ~ names ~ ":" ~ baseType) => Property(names, baseType, description = docList.mkString(" "))
	    case (docList ~ Some("=") ~ names ~ ":" ~ baseType) => Property(names, baseType, unique = true, description = docList.mkString(" "))
	    case (docList ~ Some("?") ~ names ~ ":" ~ baseType) => Property(names, baseType, optional = true, description = docList.mkString(" "))
	}
	lazy val modifier = "=" | "?"
	lazy val description: Parser[String] = "--.+\n".r ^^ {
		case d => d.drop(2).trim
	}
	lazy val typeval: Parser[GraphType] = (("Integer" ~ (isize?) ~ (intdefault?)) | ("String" ~ (ssize?)) | unqualifiedName) ^^ {
	    case "Integer" ~ None ~ (default: Option[BigInt]) => IntType(None, default)
	    case "Integer" ~ Some(size: (BigInt, BigInt)) ~ (default: Option[BigInt]) => IntType(Some(size), default)
	    case "String" ~ None => UnsizedStringType
	    case "String" ~ Some(size: (BigInt, BigInt)) => SizedStringType(size._1, size._2)
	    case "Date" => CalendarType
	    case "DateTime" => ClockType
	    case "Boolean" => BooleanType
	    case name: String => UserType(new Name(name))
	}
	lazy val intdefault: Parser[BigInt] = "=" ~> anyint ^^ {
		case i => BigInt(i)
	}
	lazy val isize: Parser[(BigInt, BigInt)] = ("[" ~> anyint ~ ".." ~ anyint <~ "]") ^^ {
	    case (min ~ ".." ~ max) => (BigInt(min), BigInt(max))
	}
	lazy val ssize: Parser[(BigInt, BigInt)] = ("[" ~> (((posint?) ~ ":"?) ~ posint) <~ "]") ^^ {
	    case (None ~ n) => val i = BigInt(n); (i, i)
	    case (Some(None ~ ":") ~ max) => (BigInt(0), BigInt(max))
	    case (Some(Some(min) ~ ":") ~ max) => (BigInt(min), BigInt(max))
	}
	lazy val dateRange: Parser[(String, String)] = ("[" ~> isoDate ~ ":" ~ isoDate <~ "]") ^^ {
		case (d1 ~ ":" ~ d2) => ("1700-01-01", "2500-01-01")
	}
	lazy val isoDate = "[-0-9:T ]+".r
	lazy val posint = "[0-9]+".r
	lazy val anyint = "[+-]?[0-9]+".r
	
	lazy val relateClause: Parser[(Relationship,List[Property])] = ("{" ~> endpoint ~ cardinalityPair ~ endpoint ~ (("," ~> rep1sep(property, ","))?) <~ "}") ^^ {
	    case (e1 ~ c ~ e2 ~ props) =>
	    	val left = Endpoint(c._1, e1._1, e1._2, c._2 == '>')
	    	val right = Endpoint(c._3, e2._1, e2._2, c._2 == '<')
	        (new Relationship(left, right), props.getOrElse(Nil))
	}
	lazy val endpoint: Parser[(Participant,Option[Names])] = ((label ~ ":"?) ~ unqualifiedName) ^^ {
	    case (None ~ id) => (er.getParticipant(new Name(id)), None)
	    case (Some(names ~ ":") ~ id) => (er.getParticipant(new Name(id)), Some(names))
	}
	lazy val cardinalityPair: Parser[(Cardinality,Char,Cardinality)] = (count ~ "[-<>]".r ~ count) ^^ {
	    case (c1 ~ dash ~ c2) => (Cardinality(c1), dash.charAt(0), Cardinality(c2))
	}
	lazy val count = "1" | "#?" | multiplicity
	lazy val multiplicity = "?" | "*" | "+" | "#"
	lazy val groupId = ("groupId" ~> qualifiedName) ^^ {
	    case id => er.groupId = Some(id); id
	}
  lazy val artifactId = ("artifactId" ~> qualifiedName) ^^ {
	    case id => er.artifactId = Some(id); id
	}
  	lazy val version = ("version" ~> "[A-Za-z0-9.]+".r) ^^ {
	    case id => er.version = Some(id.trim); id
	}
	lazy val name = ("name" ~> unqualifiedName) ^^ {
		case name => er.name = Some(new Name(name))
	}
  lazy val attribute = name | groupId | artifactId | version
	lazy val entity: Parser[Participant] = ("entity" ~> label ~ (ssize?) ~ entityClause) ^^ {
	    case (names ~ size ~ properties) => {
	    	val p = er.getParticipant(names.singular)
	    	if (p.isDefined) throw new RuntimeException("Participant " + names.singular.original + "defined twice")
	    	else p.isDefined = true
	    	p.names = Some(names)
	    	p.maxInstances = size.map(_._2)
	    	properties foreach {p.addProperty(_, er)}
	    	p
	    }
	}
	lazy val relate: Parser[Participant] = ("relate" ~> (label?) ~ (ssize?) ~ relateClause) ^^ {
	    case (names ~ size ~ clause) =>
	    	val (r, properties) = clause
	    	val p = names match {
	    		case Some(n) => er.getParticipant(n.singular)
	    		case None => er.createParticipant()
	    	}
	    	p.names = names
	    	names.map(n => r.name = n.singular)
	    	p.maxInstances = size.map(_._2)
	    	p.relationship = Some(r)
	    	r.left.participant.inRelationship(r)
	    	r.right.participant.inRelationship(r)
	    	properties foreach {p.addProperty(_, er)}
	    	p
	}
	lazy val graph = ((attribute)*) ~ ((entity | relate)*)
}