package org.catalogueoflife.e2.schema.parse

import scala.collection.mutable.Map


trait GraphType
case object UnsizedStringType extends GraphType {
	override def toString = "String"
}
case class SizedStringType(minLength: BigInt, maxLength: BigInt) extends GraphType {
	override def toString(): String = {
		"String" + (
			(minLength, maxLength) match {
				case (min, max) if min == max => "[" + max + "]"
				case (min, max) if min equals 0 => "[:" + max + "]"
				case (min, max) => "[" + min + ":" + max + "]"
			}
		)
	}
}
case class IntType(minMax: Option[(BigInt, BigInt)], default: Option[BigInt] = None) extends GraphType {
	override def toString(): String = {
		"Integer" + (
			minMax match {
				case None => ""
				case Some((min, max)) => "[" + min + ".." + max + "]"
			}
		) + default.map{"=" + _}
	}
}
case object CalendarType extends GraphType {
	override def toString(): String = {
		"Date"
	}
}
case object ClockType extends GraphType {
	override def toString(): String = {
		"DateTime"
	}
}
case object BooleanType extends GraphType {
	override def toString(): String = {
		"Boolean"
	}
}
case class UserType(name: Name) extends GraphType {
	override def toString(): String = {
		name.original
	}
}

class Name(val original: String) {
	lazy val canonical: List[String] = canonicalize(original)		// ["get", "asn1", "string"]
	lazy val lowerCase = canonical.mkString("_")					// get_asn1_string
	lazy val upperCase = canonical.map(_ toUpperCase).mkString("_")	// GET_ASN1_STRING
	lazy val pascalCase = canonical.map(_ capitalize).mkString		// GetAsn1String
	lazy val camelCase = {											// getAsn1String
		val cc = pascalCase
		cc(0).toLower + cc.substring(1, cc.length)
	}
	lazy val titleText = canonical.map(_ capitalize).mkString(" ")  // Get Asn1 String
    def +(that: Name): Name = new Name(lowerCase + "_" + that.lowerCase)
	def +(that: String): Name = this + new Name(that)
	override def equals(other: Any): Boolean = other match {
		case that: Name => this.canonical.equals(that.canonical)
		case _ => false
	}
	override def hashCode = this.canonical.hashCode
	override def toString: String = getClass.getName + "(" + canonical.toString + ")"
	def canonicalize(name: String): List[String] = {
		// lots_of_underscores => List("lots", "of", "underscores")
		// camelCase => List("camel", "case")
		// anISOStandard => List("an", "iso", "standard")
		// rule: break on _ or lower-to-upper or upper-to-upper-lower, lowercase all resulting words
		// also GetASN1String => List("get", "asn1", "string")
		// add to rule that digits always match any-case
		accumulateCanonical(name, List(""))
	}
	private def accumulateCanonical(name: Seq[Char], canonAcc: List[String]) : List[String] = {
		name match {
			case Seq('_', rest @ _*) =>
				canonAcc match {
					// keep any initial underscore
					case "" :: Nil => accumulateCanonical(rest, "_" :: Nil)
					// start a new word for any other underscores
					// if the list already contains an empty new word, do nothing
					case "" :: _ => accumulateCanonical(rest, canonAcc)
					// otherwise, add a new empty word
					case _ => accumulateCanonical(rest, "" :: canonAcc)
				}
			case Seq(l, u, rest @ _*) if (l.isLower || l.isDigit) && u.isUpper =>
				accumulateCanonical(rest, u.toLower.toString :: (canonAcc.head + l) :: canonAcc.tail)
			case Seq(u, l, rest @ _*) if (u.isUpper || u.isDigit) && l.isLower =>
				// Matches:
				// 1. IsDigit - characters 1,2
				// 2. IsDigit - characters 3,4
				// 3. isISOStandard - characters 6,7
				// but 2. already handled by previous case, so only need to handle 1. and 3.
				canonAcc match {
					// in case 3, the head must already have characters, so empty head means we are case 1
					case "" :: tail => accumulateCanonical(rest, (u.toLower.toString + l) :: tail)
					// otherwise, case 3 - start new word with these two characters
					case _ => accumulateCanonical(rest, (u.toLower.toString + l) :: canonAcc)
				}
			case Seq(u, rest @ _*) if u.isUpper =>
				// an upper case letter in a sequence of upper case letters
				accumulateCanonical(rest, (canonAcc.head + u.toLower) :: canonAcc.tail)
			case Seq(l, rest @ _*) => accumulateCanonical(rest, (canonAcc.head + l) :: canonAcc.tail)
			case Seq() => canonAcc reverse
		}
	}
}
case class Names(singular: Name, plural: Option[Name] = None)

case class Property(names: Names, baseType: GraphType, optional: Boolean = false, unique: Boolean = false,
		description: String = "") {
	override def toString() : String = {
		val sb = new StringBuilder()
		if (optional) sb.append("? ")
		if (unique) sb.append("= ")
		sb.append(names.singular.original + names.plural.map{ "/" + _ }.getOrElse("") + ": " + baseType)
		sb.toString
	}
}

trait Cardinality
case object ZeroOrOne extends Cardinality {
	override def toString = "?"
}
case object One extends Cardinality {
	override def toString = "1"
}
case object ZeroOrMore extends Cardinality {
	override def toString = "*"
}
case object OneOrMore extends Cardinality {
	override def toString = "+"
}
case object OrderedZeroOrMore extends Cardinality {
	override def toString = "#?"
}
case object OrderedOneOrMore extends Cardinality {
	override def toString = "#"
}
object Cardinality {
	def apply(c: String) : Cardinality = {
		c match {
			case "?" => ZeroOrOne
			case "1" => One
			case "*" => ZeroOrMore
			case "+" => OneOrMore
			case "#?" => OrderedZeroOrMore
			case "#" => OrderedOneOrMore
		}
	}
}

// An entity is a participant with no 'relationship'
// A simple relationship has no properties or relationships
// A relationship which exists as a separate entity is one that has properties or is an endpoint of a relationship
class Participant(val id: Name) {
	var isDefined = false
	var names: Option[Names] = None
	override def toString = "Participant(" + id.original + ")"
	// entity behaviours
	var properties: List[Property] = Nil
	var relationships: List[Relationship] = Nil // Relationships of which I am an endpoint
	var maxInstances: Option[BigInt] = None
	// relationship behaviours
	var relationship: Option[Relationship] = None
	def addProperty(p: Property, graph: ERGraph) = {
		p.baseType match {
			case UserType(name) => {
				val container = Endpoint(if (p.unique) ZeroOrOne else ZeroOrMore, this, None, false)
				val containedPart = graph.getParticipant(name)
				val contained = Endpoint(if (p.optional) ZeroOrOne else One, containedPart, Some(p.names), true)
				val relPart = graph.createParticipant()
				val r = new Relationship(container, contained)
				relPart.relationship = Some(r)
				inRelationship(r)
				containedPart.inRelationship(r)
			}
			case _ => properties = p :: properties
		}
	}
	def inRelationship(r: Relationship) {
		relationships = r :: relationships
	}
}

case class Endpoint(val cardinality: Cardinality, val participant: Participant, val names: Option[Names] = None,
		val contained: Boolean) {
	def refName = names.getOrElse(participant.names.get).singular
	def refCollectionName = {
		val n = names.getOrElse(participant.names.get)
		n.plural.getOrElse(n.singular + "collection")
	}
}
class Relationship(val left: Endpoint, val right: Endpoint) {
	override def toString = "Relationship(" + left + ", " + right + ")"
	lazy val lower: Endpoint = (left.cardinality, right.cardinality) match {
		case (OrderedOneOrMore, OrderedZeroOrMore) | (OrderedOneOrMore, OneOrMore) | (OrderedOneOrMore, ZeroOrMore)
	    	| (OrderedOneOrMore, One) | (OrderedOneOrMore, ZeroOrOne) | (OrderedZeroOrMore, OneOrMore)
	    	| (OrderedZeroOrMore, ZeroOrMore) | (OrderedZeroOrMore, One) | (OrderedZeroOrMore, ZeroOrOne) | (OneOrMore, ZeroOrMore) 
	    	| (OneOrMore, One) | (OneOrMore, ZeroOrOne) | (ZeroOrMore, One) | (ZeroOrMore, ZeroOrOne) | (One, ZeroOrOne) => right
		case _ => left
	}
	lazy val higher = if (lower == left) right else left
	lazy val contained: Endpoint = if (left.contained) left else right
	lazy val container: Endpoint = if (left.contained) right else left
	lazy val hasContainer = left.contained || right.contained
	private var givenNames: Option[Names] = None
	def names_=(names: Names) = {
		givenNames = Some(names)
	}
	def names = givenNames.getOrElse(defaultNames)
	lazy val defaultNames: Names = {
		if (hasContainer) {
			val containedName = contained.cardinality match {
				case ZeroOrOne | One => contained.refName
				case _ => contained.refCollectionName
			}
			Names(container.refName + contained.refName, Some(container.refName + contained.refCollectionName))
		}
		else {
			val leftName = left.cardinality match {
				case ZeroOrOne | One => left.refName
				case _ => left.refCollectionName
			}
			val rightName = right.cardinality match {
				case ZeroOrOne | One => right.refName
				case _ => right.refCollectionName
			}
			Names(left.refName + right.refName + "map", None)
		}
	}
  def name = {
    if (hasContainer) {
      contained.cardinality match {
        case ZeroOrOne | One => names.singular
        case _ => names.plural.get
      }
    }
    else names.singular
  }
}

class ERGraph {

	var name: Option[Name] = None
  var groupId: Option[String] = None
  var artifactId: Option[String] = None
  var version: Option[String] = None
	var participants = Map[Name,Participant]()

	var lock: AnyRef = new Object
	var id: Int = 0
	def nextId: Int = lock.synchronized {
		id += 1
		id
	}
	def createParticipant() = {
		val name = new Name("_anon_#" + nextId)
		val p = new Participant(name)
		participants(name) = p
		p
	}
	def getParticipant(name: Name): Participant = {
		participants.getOrElseUpdate(name, new Participant(name))
	}
	def print() : Unit = {
		participants.values foreach println
	}
	def validate() : Unit = {
		// Check that all entities are defined (an undefined entity occurs if a relate clause references
		// an entity, but there is no entity clause)
	}
}

