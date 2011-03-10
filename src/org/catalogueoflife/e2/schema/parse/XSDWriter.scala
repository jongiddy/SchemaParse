package org.catalogueoflife.e2.schema.parse

import scala.collection.mutable.HashMap

/*
 * For XSD, # is same as + and #? is same as *
 * A x_? A: type A contains type Aid minOccurs=0
 * A x_1 A: type A contains type AId
 * A x<* A: type A contains type AIdSeq minOccurs=0
 * A x<+ A: type A contains type AIdSeq
 * A ?1<? B: type A contains type B minOccurs=0 ; type B contains type AId minOccurs=0 (allows linking to other containers)
 * A ?1<1 B: type A contains type B ; type B contains type AId minOccurs=0
 * A ?1<* B: type A contains type BSeq minOccurs=0 ; type B contains type AId minOccurs=0
 * A ?1<+ B: type A contains type BSeq ; type B contains type AId minOccurs=0
 * Note: in 1<y relationship, B.AId is always minOccurs=0 because it may be obvious from the structure
 * e.g. Source 1<* Taxon, Node *<? Taxon
 *     <Source><Taxon>: taxon does not need to indicate sourceID as it is obvious
 *     <Node><Taxon>: taxon can indicate sourceID
 *     Taxon never indicates which Nodes it is related to
 * A *+<? B: type A contains type B minOccurs=0
 * A *+<1 B: type A contains type B
 * A *+<* B: type A contains type BSeq minOccurs=0
 * A *+<+ B: type A contains type BSeq
 * A x-? B: type A contains type BId minOccurs=0
 * A x-1 B: type A contains type BId
 * 
 */

class XSDWriter {

	trait BaseType
	trait XSDType extends BaseType
	case object XSDInteger extends XSDType {
		override def toString = "integer"
	}
	case object XSDString extends XSDType {
		override def toString = "string"
	}
	case object XSDDate extends XSDType {
		override def toString = "date"
	}
	case object XSDDateTime extends XSDType {
		override def toString = "integer"             // was datetime, changed to get things working with SCA
	}
	case object XSDBoolean extends XSDType {
		override def toString = "boolean"
	}

	object XSDType {
		def apply(graphType: GraphType) : XSDType = {
			graphType match {
				case _ : IntType => XSDInteger
				case SizedStringType(min, max) => XSDString
				case UnsizedStringType => XSDString
				case _ : BooleanType => XSDBoolean
				case CalendarType => XSDDate
				case ClockType => XSDDateTime
			}
		}
	}
	class UserType(val names: Names) extends BaseType {
		override def toString = "tns:" + typeStyle(names.singular)
    var attributes: List[Attribute] = Nil
    def addAttribute(attr: Attribute) = attributes = attr :: attributes
	}
	class ComplexType(names: Names) extends UserType(names) {
		var elements: List[Element] = Nil
		def addElement(elem: Element) = elements = elem :: elements
	}
	class EntityType(names: Names) extends ComplexType(names) {
    var idOnlySupported = false
  }
	class SeqType(names: Names) extends ComplexType(names)
  class OrIdType(names: Names) extends ComplexType(names)
	class IdType(names: Names) extends UserType(names)

	abstract class Element(val name: Name, val tipo: BaseType, val optional: Boolean, val unbounded: Boolean, val description: String)
	class EntityElement(name: Name, tipo: BaseType, optional: Boolean=false, description: String="")
		extends Element(name, tipo, optional, false, description)
	class SeqElement(name: Name, tipo: BaseType, description: String="")
		extends Element(name, tipo, false, true, description)
	case class Attribute(name: Name, tipo: BaseType, optional: Boolean = false, default: Option[String] = None, description: String="")

	val entityTypes = new HashMap[Participant,EntityType]
	val seqTypes = new HashMap[UserType,SeqType]
  val listTypes = new HashMap[UserType,EntityType]
  val orIdTypes = new HashMap[EntityType,OrIdType]
	val idTypes = new HashMap[EntityType,IdType]
  var targetNamespace: String = "unknown"

	def analyse(er: ERGraph) = {
    for (groupId <- er.groupId;
      artifactId <- er.artifactId
    ) {
       targetNamespace = "http://" + groupId.split("[.]").reverse.mkString(".") + "/" + artifactId.replaceAll("[.]", "/") +
        er.version.map("/" + _.replaceAll("[.]", "/")).getOrElse("")
    }
		def seqType(itemType: UserType): SeqType = {
			seqTypes.getOrElseUpdate(itemType, {
				val t = new SeqType(Names(itemType.names.singular + "seq", None))
				t.addElement(new SeqElement(itemType.names.singular, itemType))
				t
			})
		}
    def listType(tipo: UserType): EntityType = {
			listTypes.getOrElseUpdate(tipo, {
				val t = new EntityType(Names(tipo.names.singular + "list", None))
        val seq = seqType(tipo)
				t.addElement(new EntityElement(tipo.names.plural.getOrElse(seq.names.singular), seq, optional=true))
				t
			})
		}
		def orIdType(tipo: EntityType): OrIdType = {
			orIdTypes.getOrElseUpdate(tipo, {
				val t = new OrIdType(Names(tipo.names.singular + "orId", tipo.names.plural.map(_ + "orIds")))
        t.addElement(new EntityElement(tipo.names.singular, tipo))
        val idTipo = idType(tipo)
        t.addElement(new EntityElement(idTipo.names.singular, idTipo))
				t
			})
		}
    def idType(tipo: EntityType): IdType = {
      // any type which needs an id can often benefit from a listType to
      // help in operations, for example, retrieval of all the x's
      listType(tipo)
			idTypes.getOrElseUpdate(tipo, {
				val t = new IdType(Names(tipo.names.singular + "id", Some(tipo.names.singular + "ids")))
				tipo.addAttribute(Attribute(new Name("id"), t, optional=true))
				t
			})
		}
		def handleContainedRelationship(containerRef: Endpoint, containedRef: Endpoint, orId: Boolean=false) = {
			val containerType = entityTypes(containerRef.participant)
			if (containerRef.participant == containedRef.participant) {
				// different rules for circular references (entity contains instance(s) of itself)
				val containedType = containerType
				containedRef.cardinality match {
					case ZeroOrOne =>
						containerType.addElement(new EntityElement(containedRef.refName + "id", idType(containedType), optional=true))
					case One =>
						containerType.addElement(new EntityElement(containedRef.refName + "id", idType(containedType)))
					case ZeroOrMore | OrderedZeroOrMore =>
						containerType.addElement(new EntityElement(containedRef.refName + "ids", seqType(idType(containedType)), optional=true))
					case OneOrMore | OrderedOneOrMore =>
						containerType.addElement(new EntityElement(containedRef.refName + "ids", seqType(idType(containedType))))
				}
			}
			else {
				val containedType = entityTypes(containedRef.participant)
        if (orId) {
          val addedType = orIdType(containedType)
          containedRef.cardinality match {
            case ZeroOrOne =>
              containerType.addElement(new EntityElement(containedRef.refName + "orId", addedType, optional=true))
            case One =>
              containerType.addElement(new EntityElement(containedRef.refName + "orId", addedType))
            case ZeroOrMore | OrderedZeroOrMore =>
              containerType.addElement(new EntityElement(containedRef.refCollectionName + "orIds", seqType(addedType), optional=true))
            case OneOrMore | OrderedOneOrMore =>
              containerType.addElement(new EntityElement(containedRef.refCollectionName + "orIds", seqType(addedType)))
          }
        }
        else {
          containedRef.cardinality match {
            case ZeroOrOne =>
              containerType.addElement(new EntityElement(containedRef.refName, containedType, optional=true))
            case One =>
              containerType.addElement(new EntityElement(containedRef.refName, containedType))
            case ZeroOrMore | OrderedZeroOrMore =>
              containerType.addElement(new EntityElement(containedRef.refCollectionName, seqType(containedType), optional=true))
            case OneOrMore | OrderedOneOrMore =>
              containerType.addElement(new EntityElement(containedRef.refCollectionName, seqType(containedType)))
          }
        }
				containerRef.cardinality match {
					case ZeroOrOne | One =>
						containedType.addAttribute(Attribute(containerRef.refName + "id", idType(containerType), optional=true))
					case _ =>
				}
			}
		}
		def handlePeerRelationship(peer1: Endpoint, peer2: Endpoint) = {
			peer2.cardinality match {
				case ZeroOrOne =>
					entityTypes(peer1.participant).addAttribute(new Attribute(peer2.refName + "id", idType(entityTypes(peer2.participant)), optional=true))
				case One =>
					// also make optional
					entityTypes(peer1.participant).addAttribute(new Attribute(peer2.refName + "id", idType(entityTypes(peer2.participant)), optional=true))
				case _ =>
			}
		}
		def handleRelationship(r: Relationship) {
			if (r.hasContainer) handleContainedRelationship(r.container, r.contained)
			else {
				handlePeerRelationship(r.left, r.right)
				handlePeerRelationship(r.right, r.left)
			}
		}
		for (e <- er.participants.values;
			if !(e.properties.isEmpty && e.relationships.isEmpty)
		) {
			val t = new EntityType(Names(e.names.map(_.singular).getOrElse(e.relationship.get.names.singular),
        e.names.map(_.plural).getOrElse(e.relationship.get.names.plural)))
			for (p <- e.properties.reverse) {
				p.baseType match {
					case BooleanType(None) =>
						t.addAttribute(new Attribute(p.names.singular, XSDType(p.baseType), optional=p.optional,
              description=p.description))
          case BooleanType(Some(default)) =>
						t.addAttribute(new Attribute(p.names.singular, XSDType(p.baseType), optional=true,
              default=Some(if (default) "true" else "false"), description=p.description))
					case IntType(_, Some(default)) =>
						t.addAttribute(new Attribute(p.names.singular, XSDType(p.baseType), optional=true,
              default=Some(default.toString), description=p.description))
					case _ =>
						t.addElement(new EntityElement(p.names.singular, XSDType(p.baseType), optional=p.optional,
              description=p.description))
				}
			}
			entityTypes(e) = t
		}
		for (e <- er.participants.values;
			if !(e.properties.isEmpty && e.relationships.isEmpty);
			if (e.relationship.isDefined)
		) {
			// RelationshipEntities contain a relationship between two other entities
			// need to convert this to two relationships
			// A *>+ B becomes
			// A 1>+ me and me *>1 B
			val r = e.relationship.get
			val leftA = r.left.copy(cardinality = One)
			val rightA = r.right.copy(participant=e)
			println(leftA, rightA)
			handleRelationship(new Relationship(leftA, rightA))
			val leftB = r.left.copy(participant=e)
			val rightB = r.right.copy(cardinality = One)
			handleRelationship(new Relationship(leftB, rightB))
      if (r.hasContainer) {
        val containedType = entityTypes(r.contained.participant)
        idType(containedType) // ensure contained type has an id type
        containedType.idOnlySupported = true
      }
		}
		for (e <- er.participants.values;
			if e.properties.isEmpty && e.relationships.isEmpty
		) {
			handleRelationship(e.relationship.get)
		}
    for (e <- er.participants.values;
      if !(e.properties.isEmpty && e.relationships.isEmpty);
      if (e.relationship.isDefined)
    ) {
      // as a final step, any relationship-entities that do not have an idType get one created here
      val r = e.relationship.get
      val tipo = entityTypes(e)
			idTypes.getOrElseUpdate(tipo, {
		    val t = new IdType(Names(tipo.names.singular + "id", Some(tipo.names.singular + "ids")))
			  t.addAttribute(new Attribute(r.left.refName + "id", idType(entityTypes(r.left.participant))))
			  t.addAttribute(new Attribute(r.right.refName + "id", idType(entityTypes(r.right.participant))))
        t
			})
    }
	}
	def typeStyle(name: Name) = name.pascalCase
	def tagStyle(name: Name) = name.camelCase

	def write(out: java.io.Writer) {
		out.write(toString)
	}
	override def toString = {
		val sb = new StringBuilder()
		var indentLevel=0
		def indent = sb.append(" " * 3 * indentLevel)
		def appendElements(elements: List[Element]) = {
			for (elem <- elements) {
				indent.append("<element name='").append(tagStyle(elem.name)).append("' type='").append(elem.tipo).append("'")
				if (elem.optional) sb.append(" minOccurs='0'")
				if (elem.unbounded) sb.append(" maxOccurs='unbounded'")
				if (elem.description.length > 0) {
					sb.append("><annotation>\n")
					indentLevel += 1
					indent.append("<documentation>").append(elem.description)
					sb.append("</documentation>")
					sb.append("</annotation>")
					sb.append("</element>\n")
					indentLevel -= 1
				}
				else sb.append("/>\n")
			}
		}
		def appendAttributes(attributes: List[Attribute]) = {
			for (attr <- attributes) {
				indent.append("<attribute name='").append(tagStyle(attr.name)).append("' type='").append(attr.tipo).append("'")
				attr.default.map(sb.append(" default='").append(_).append("'"))
				if (!attr.optional) sb.append(" use='required'")
				if (attr.description.length > 0) {
					sb.append("><annotation>\n")
					indentLevel += 1
					indent.append("<documentation>").append(attr.description)
					sb.append("</documentation>")
					sb.append("</annotation>")
					sb.append("</attribute>\n")
					indentLevel -= 1
				}
				else sb.append("/>\n")
			}
		}
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    sb.append("<schema targetNamespace='" + targetNamespace + "'\n")
    sb.append("        xmlns='http://www.w3.org/2001/XMLSchema'\n")
    sb.append("        xmlns:tns='" + targetNamespace + "'\n")
    sb.append("        elementFormDefault='qualified'\n")
    sb.append("        attributeFormDefault='unqualified'>\n")
    indentLevel += 1
    val a = collection.mutable.ArrayBuffer.concat(entityTypes.values, seqTypes.values, listTypes.values,
      orIdTypes.values, idTypes.values).sortWith(
      (t1:UserType, t2:UserType) => t1.names.singular.lowerCase < t2.names.singular.lowerCase)

		for (tipo <- a) {
			sb.append("\n")
			tipo match {
				case t : EntityType =>
					indent.append("<complexType name='").append(typeStyle(t.names.singular)).append("'>\n")
					indentLevel += 1
					if (t.elements.size > 0) {
						indent.append("<all")
            if (t.idOnlySupported) sb.append(" minOccurs='0'")
            sb.append(">\n")
						indentLevel += 1
						appendElements(t.elements.reverse.filter(elem => !elem.optional).sortWith(
								(elem1:Element, elem2:Element) => elem1.tipo.toString < elem2.tipo.toString))
						appendElements(t.elements.reverse.filter(elem => elem.optional).sortWith(
								(elem1:Element, elem2:Element) => elem1.tipo.toString < elem2.tipo.toString))
						indentLevel -= 1
						indent.append("</all>\n")
					}
					appendAttributes(t.attributes.reverse)
					indentLevel -= 1
					indent.append("</complexType>\n")
				case t : SeqType =>
					indent.append("<complexType name='").append(typeStyle(t.names.singular)).append("'>\n")
					indentLevel += 1
					if (t.elements.size > 0) {
						indent.append("<sequence>\n")
						indentLevel += 1
						appendElements(t.elements.reverse)
						indentLevel -= 1
						indent.append("</sequence>\n")
					}
					appendAttributes(t.attributes.reverse)
					indentLevel -= 1
					indent.append("</complexType>\n")
        case t : OrIdType =>
          indent.append("<complexType name='").append(typeStyle(t.names.singular)).append("'>\n")
          indentLevel += 1
					if (t.elements.size > 0) {
						indent.append("<choice>\n")
						indentLevel += 1
						appendElements(t.elements.reverse)
						indentLevel -= 1
						indent.append("</choice>\n")
					}
					appendAttributes(t.attributes.reverse)
					indentLevel -= 1
					indent.append("</complexType>\n")
				case t : IdType =>
					indent.append("<complexType name='").append(typeStyle(t.names.singular)).append("'>\n")
					indentLevel += 1
          indent.append("<simpleContent>\n")
          indentLevel += 1
          indent.append("<extension base='string'>\n")
          indentLevel += 1
          appendAttributes(t.attributes.reverse)
          indentLevel -= 1
          indent.append("</extension>\n")
          indentLevel -= 1
          indent.append("</simpleContent>\n")
					indentLevel -= 1
					indent.append("</complexType>\n")
			}
		}
		indentLevel -= 1
		sb.append("</schema>\n")
		sb.toString
	}
}
