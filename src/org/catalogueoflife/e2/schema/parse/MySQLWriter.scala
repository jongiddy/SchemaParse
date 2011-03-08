package org.catalogueoflife.e2.schema.parse

import scala.collection.mutable.HashMap

class MySQLWriter {

	// for Base Schema
	/*
	val tableNameStyle = (name: Name) => name.lowerCase
	val fieldNameStyle = (name: Name) => name.lowerCase
	val sqlStyle = (text: String) => text.toUpperCase
	val sqlTypeStyle = (text: String) => text.toLowerCase
	*/
	// for Multitree Schema
	val tableNameStyle = (name: Name) => name.pascalCase
	val fieldNameStyle = (name: Name) => name.camelCase
	val sqlStyle = (text: String) => text.toLowerCase
	val sqlTypeStyle = (text: String) => text.toLowerCase

	def escapeIdentifier(word: String) = "`" + word + "`"

	// types are suitable for use with MySQL 5.0.3 and above
	trait MySQLDatabaseType
	trait MySQLIntegralType extends MySQLDatabaseType
	case object MySQLTinyIntUnsigned extends MySQLIntegralType {
		override def toString = "tinyint unsigned"
	}
	case object MySQLTinyIntSigned extends MySQLIntegralType {
		override def toString = "tinyint"
	}
	case object MySQLSmallIntUnsigned extends MySQLIntegralType {
		override def toString = "smallint unsigned"
	}
	case object MySQLSmallIntSigned extends MySQLIntegralType {
		override def toString = "smallint"
	}
	case object MySQLMediumIntUnsigned extends MySQLIntegralType {
		override def toString = "mediumint unsigned"
	}
	case object MySQLMediumIntSigned extends MySQLIntegralType {
		override def toString = "mediumint"
	}
	case object MySQLIntUnsigned extends MySQLIntegralType {
		override def toString = "integer unsigned"
	}
	case object MySQLIntSigned extends MySQLIntegralType {
		override def toString = "integer"
	}
	case object MySQLBigIntUnsigned extends MySQLIntegralType {
		override def toString = "bigint unsigned"
	}
	case object MySQLBigIntSigned extends MySQLIntegralType {
		override def toString = "bigint"
	}
	trait MySQLTextualType extends MySQLDatabaseType
	case class MySQLCharType(size: Int) extends MySQLTextualType {
		override def toString = "char(" + size + ")"
	}
	trait MySQLVarCharOrTextType extends MySQLTextualType
	case class MySQLVarCharType(max: Int) extends MySQLVarCharOrTextType {
		override def toString = "varchar(" + max + ")"
	}
	case object MySQLTextType extends MySQLVarCharOrTextType {
		override def toString = "text"
	}
	case object MySQLMediumTextType extends MySQLTextualType {
		override def toString = "mediumtext"
	}
	case object MySQLLongTextType extends MySQLTextualType {
		override def toString = "longtext"
	}
	case object MySQLDateType extends MySQLDatabaseType {
		override def toString = "date"
	}
	case object MySQLDateTimeType extends MySQLDatabaseType {
		override def toString = "datetime"
	}
	case object MySQLBoolType extends MySQLDatabaseType {
		override def toString = "boolean"
	}
	object MySQLDatabaseType {
		def apply(graphType: GraphType) : MySQLDatabaseType = {
			def twoto(power: Int) = BigInt(2) << (power - 1)
			graphType match {
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(8) => MySQLTinyIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(8-1) && max < twoto(8-1) => MySQLTinyIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(16) => MySQLSmallIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(16-1) && max < twoto(16-1) => MySQLSmallIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(24) => MySQLMediumIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(24-1) && max < twoto(24-1) => MySQLMediumIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(32) => MySQLIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(32-1) && max < twoto(32-1) => MySQLIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(64) => MySQLBigIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(64-1) && max < twoto(64-1) => MySQLBigIntSigned
				case IntType(None, _) => MySQLBigIntSigned
				case SizedStringType(min, max) if min == max && max < twoto(8) => MySQLCharType(max.intValue)
				// although varchar can be up to 2^16-1, that is also the entire row limit,
				// so assume we handle no more than 2^12
				// TODO - make it a MySQLVarCharOrTextType, and then calculate row sizes before
				// deciding
				case SizedStringType(min, max) if max < twoto(12) => MySQLVarCharType(max.intValue)
				case SizedStringType(min, max) if max < twoto(16) => MySQLTextType
				case SizedStringType(min, max) if max < twoto(24) => MySQLMediumTextType
				case SizedStringType(min, max) if max < twoto(32) => MySQLLongTextType
				case UnsizedStringType => MySQLLongTextType
				case CalendarType => MySQLDateType
				case ClockType => MySQLDateTimeType
				case BooleanType => MySQLBoolType
				case _ => throw new RuntimeException("cannot convert type")
			}
		}
	}

	trait MySQLAction
	case object Restrict extends MySQLAction {
		def asString = Restrict
	}
	case object Cascade extends MySQLAction {
		def asString = Cascade
	}
	case object SetNull extends MySQLAction {
		def asString = SetNull
	}
	
	var handled = Set[Relationship]();
	case class Field(name: Name, baseType: MySQLDatabaseType, notNull: Boolean=false, unique: Boolean=false, autoIncrement: Boolean=false,
			references: Option[(Table,MySQLAction)]=None, description: String="") {
		def originalName: Name = references match {
			case None => name
			case Some((table,_)) => table.primaryKey.originalName
		}
		lazy val nameAsString = escapeIdentifier(fieldNameStyle(name))
		def isContainer = references.map(_._2) match {
			case Some(Cascade) => true
			case Some(SetNull) => true
			case _ => false
		}
		def createClause(sb: StringBuilder) = {
			sb.append(nameAsString).append(" " * math.max(21-nameAsString.length, 1))
			sb.append(sqlTypeStyle(baseType.toString))
			if (unique) sb.append(sqlStyle(" unique"))
			if (notNull) sb.append(sqlStyle(" not null"))
			if (autoIncrement) sb.append(sqlStyle(" auto_increment"))
			references.map { ref =>
				val (table, onDeleteAction) = ref
				sb.append("\n").append(" " * 24)
				sb.append(sqlStyle(" references ")).append(table.nameAsString).append(table.keyFields)
				if (!table.hasDedicatedKey) sb.append(sqlStyle(" on update cascade"))
				if (onDeleteAction != Restrict) sb.append(sqlStyle(" on delete " + onDeleteAction))
			}
			if (!description.isEmpty) {
				sb.append("\n").append(" " * 24)
				sb.append(sqlStyle(" comment '")).append(description).append("'")
			}
		}
	}
	class Table(participant: Participant, val name: Name) {
		lazy val nameAsString = escapeIdentifier(tableNameStyle(name))
		private var key: Option[Field] = None
		private var keyCanChange: Boolean = true // until it has been retrieved, the key can be changed
		var hasDedicatedKey = false // indicates whether the key field was explicitly created to be a key (is not a data field)
		private var myfields: List[Field] = Nil
		def fields = myfields
		def keyFields = "(" + primaryKey.nameAsString + ")"
		def hasPrimaryKey: Boolean = ! key.isEmpty
		def primaryKey: Field = getKey(Set[Table](this))
		var uniqueKeys: List[List[Field]] = Nil // each List[Field] is an ordered set of fields that make a unique key
		var indexKeys: List[List[Field]] = Nil // each List[Field] is an ordered set of fields to be indexed
		private def setKey(field: Field) = {
			key = Some(field)
			hasDedicatedKey = field.references match {
				case None => false
				case Some((table, action)) => table.hasDedicatedKey
			}
		}
		def addField(field: Field) {
			myfields = field :: myfields
			// Check if field is suitable to be a primary key
			if (keyCanChange && field.unique && field.notNull) {
				field.baseType match {
					case _: MySQLIntegralType =>
						// always use an integer over a string
						key map { _.baseType } match {
							case None | Some(_ : MySQLCharType) | Some(_ : MySQLVarCharType)=>
								setKey(field)
							case _ =>
						}
					case MySQLCharType(size) if size <=8 =>
						// if no int key, use shortest String as long as length is <= 8
						key map { _.baseType } match {
							case None =>
								setKey(field)
							case List(MySQLCharType(ksize)) if size <= ksize =>
								setKey(field)
							case List(MySQLVarCharType(ksize)) if size <= ksize =>
								setKey(field)
							case _ =>
						}
					case MySQLVarCharType(size) if size <=8 =>
						// if no int key, use shortest String as long as length is <= 8
						key map { _.baseType } match {
							case None =>
								setKey(field)
							case List(MySQLCharType(ksize)) if size <= ksize =>
								setKey(field)
							case List(MySQLVarCharType(ksize)) if size <= ksize =>
								setKey(field)
							case _ =>
						}
					case _ =>
				}
			}
		}
		def generateContainedKey(): Unit = generateContainedKey(Set[Table](this))
		def generateContainedKey(seen: Set[Table]): Unit = {
			for (r <- participant.relationships;
				if r.hasContainer;
				if r.contained.cardinality == ZeroOrOne;
				if r.container.cardinality == One;
				if tables(r.contained.participant) == this;
				if !seen(tables(r.container.participant))
			) {
				if (key isEmpty) {
					val containerTable = tables(r.container.participant);
					val containerKey = containerTable.getKey(seen + containerTable);
					val fieldName = r.container.refName + containerKey.originalName
					setKey(Field(fieldName, containerKey.baseType, unique=true, notNull=true, references=Some((containerTable, Cascade))))
					handled = handled + r
				}
			}
		}
		private def getKey(seen: Set[Table]): Field = {
			// given a table, return a field suitable for use as a key
			// seen is a list of tables already seen during this operation (to avoid loops)
			if (key.isEmpty) {
				generateContainedKey(seen)
				if (key isEmpty) {
					val keyType = participant.maxInstances match {
						case None => MySQLIntUnsigned
						case Some(max) => MySQLDatabaseType(IntType(Some((BigInt(0), max))))
					}
					setKey(Field(new Name("id"), keyType, notNull=true, unique=true, autoIncrement=true))
					hasDedicatedKey = true
				}
				// At this point, we are certain that key is defined
				myfields = key.get :: myfields
			}
			keyCanChange = false
			key.get
		}
		def doOrdering(fields: List[Field], acc: Array[List[Field]]) : Array[List[Field]] = {
			fields match {
				case Nil => acc
				case field :: rest if List(field) == key => acc(0) = field :: acc(0) ; doOrdering(rest, acc)
				case field :: rest if field.unique && field.notNull => acc(1) = field :: acc(1) ; doOrdering(rest, acc)
				case field :: rest if field.isContainer => acc(2) = field :: acc(2) ; doOrdering(rest, acc)
				case field :: rest => acc(3) = field :: acc(3) ; doOrdering(rest, acc)
			}
		}
		def createClause(sb: StringBuilder) = {
			// the fields are created in reverse order, then relationships are added to the head
			// we reverse the list of fields in order to make the table define fields in order
			val ordered = doOrdering(fields, Array[List[Field]](Nil,Nil,Nil,Nil))
			sb.append(sqlStyle("create table ")).append(nameAsString).append(" (")
			var first = true
			for (fields <- ordered;
				field <- fields
			) {
				if (first) {
					first = false
					sb.append("\n  ")
				}
				else sb.append(",\n  ")
				field.createClause(sb)
			}
			if (! key.isEmpty) {
				sb.append(sqlStyle(",\n  primary key ")).append(keyFields)
			}
			for (keyFields <- uniqueKeys) {
				sb.append(sqlStyle(",\n  unique (")).append(keyFields.map(_.nameAsString).mkString(", ")).append(")")
			}
			for (keyFields <- indexKeys) {
				sb.append(sqlStyle(",\n  index (")).append(keyFields.map(_.nameAsString).mkString(", ")).append(")")
			}
			sb.append("\n)").append(sqlStyle(" engine=")).append("InnoDB").append(sqlStyle(" charset=")).append("utf8;")
		}
	}

/*
	private def createTableName(e: Endpoint): Name = {
		e.cardinality match {
			case ZeroOrOne | One => {
				val refName = e.names.map(_.singular.lowerCase)
				e.names match {
					case None => e.entity.names.singular
					case Some(names) => names.singular + e.entity.names.singular
				}
			}
			case ZeroOrMore | OneOrMore => {
				e.names match {
					case None => e.entity.names.plural.getOrElse(e.entity.names.singular)
					case Some(names) => names.singular + e.entity.names.plural.getOrElse(e.entity.names.singular)
				}
			}
		}
	}
	def createMapTable(r: Relationship): Table = {
		val (container, contained) = if (r.left.contained) (r.right, r.left) else (r.left, r.right)
		val tableName = r.names match {
			case Some(names) => names.singular
			case None => {
				container.refName + (contained.cardinality match {
					case ZeroOrOne | One => contained.refName
					case _ => contained.refCollectionName
				})
			}
		}
		val table = new Table(tableName)
		val containedField = createMapField(contained, container.cardinality, Restrict)
		val containerField = createMapField(container, contained.cardinality, if (contained.contained) Cascade else Restrict)
		table.addField(containerField)
		table.addField(containedField)
		// put containerField first in index, if deletion cascades this index will speed it up
		table.uniqueKeys = List(containerField, containedField) :: table.uniqueKeys
		table
	}
	*/
	// c is the cardinality of the related entity
	def createMapField(e: Endpoint, c: Cardinality, onDeleteAction: MySQLAction) : Field = {
		val table = tables(e.participant)
		val key = table.primaryKey
		Field(e.refName + key.originalName, key.baseType, notNull=true,
				unique=(c==ZeroOrOne || c==One), references=Some((table, onDeleteAction)))
	}
	def createForeignKey(forKey: Endpoint, target: Endpoint, notNull: Boolean, unique: Boolean, onDeleteAction: MySQLAction) {
		// add a foreign key to forKey pointing to target
		val forKeyTable = tables(forKey.participant)
		val targetTable = tables(target.participant)
		val targetKey = targetTable.primaryKey
		val fieldName = target.refName + targetKey.originalName
		val field = Field(fieldName, targetKey.baseType, notNull, unique,
				references=Some((targetTable, onDeleteAction)))
		forKeyTable.addField(field)
		if (onDeleteAction == Cascade) {
			// create an index to speed up cascading deletion
			forKey.cardinality match {
				case ZeroOrOne | One =>
					// null fields are considered unique, so OK to use this for ZeroOrOne
					forKeyTable.uniqueKeys = List(field) :: forKeyTable.uniqueKeys
				case _ =>
					forKeyTable.indexKeys = List(field) :: forKeyTable.indexKeys
			}
		}
	}
	def createTableForParticipant(p: Participant): Table = {
		val tableName = p.names.map(_.singular).getOrElse(p.relationship.map(_.name).getOrElse {
			throw new RuntimeException("cannot create entity from unnamed participant: " + p)
		})
		val table = new Table(p, tableName)
		tables(p) = table
		table
	}
	def createTableForSimpleRelationship(r: Relationship): Table = {
		val p = new Participant(r.name) // we just create this as an index object for the tables map
		// since Simple Relationships are not endpoints, it does not need sensible values
		val tableName = r.name
		val table = new Table(p, tableName)
		tables(p) = table
		table
	}
	def handleSimpleRelationship(r: Relationship) : Unit = r.lower.cardinality match {
		// The general principle for representing containment relationships is that:
		// - deleting the container removes the relationship with the contained entity
		// - deleting the contained entity may be restricted without first explicitly breaking the relationship with the container
		// Generally, a foreign key in the container achieves this, but we break that for *-1 relationships where it is usual to put
		// the foreign key in the * side.
		// - deleting an entity in a non-containment relationship also requires breaking the relationship first (except in x-1
		//   relationships where it can delete the other entity as well)
		// These principles are compromised if the correct solution is not the best one for space-saving
		// XXX - need a way to break cycles
		case ZeroOrOne =>
			r.higher.cardinality match {
				case ZeroOrOne =>
					if (r.hasContainer) {
						createForeignKey(r.container, r.contained, false, true, Restrict)
					}
					else {
						// ?-? - do not delete in either direction - requires map table
						val table = createTableForSimpleRelationship(r)
						val leftField = createMapField(r.left, r.right.cardinality, Restrict)
						val rightField = createMapField(r.right, r.left.cardinality, Restrict)
						table.addField(rightField)
						table.addField(leftField)
						// both keys are unique
						table.uniqueKeys = List(rightField) :: List(leftField) :: table.uniqueKeys
					}
				case One =>
					if (r.lower.contained)
						// ?>1 - for referential integrity, we could use the same logic as for ?>?
						//createForeignKey(r.higher, r.lower, false, true, Restrict)
						// However, usual practice is to add the foreign key to the optional side
						// for memory efficiency. To remove the relationship, we remove the contained
						// entity if the container is deleted. This is permitted due to the 1-constraint.
						createForeignKey(r.lower, r.higher, true, true, Cascade)
					else if (r.higher.contained)
						// ?<1 - equivalent to ?<?, but key must be notNull
						createForeignKey(r.lower, r.higher, true, true, Restrict)
					else
						// ?-1 - add foreign key to left side - delete left entity if right entity is deleted
						createForeignKey(r.lower, r.higher, true, true, Cascade)
				case ZeroOrMore | OneOrMore =>
					// this case is restricted, since we can only really add a foreign key to the RHS,
					// so where container appears on left, use a map table instead
					if (r.lower.contained)
						// ?>*,?>+ - add nullable foreign key to right side - restrict deletion
						createForeignKey(r.higher, r.lower, false, false, Restrict)
					else if (r.higher.contained)
						// ?<*,?<+ - add nullable foreign key to right side - set null on deletion
						// could use map table to prevent deletion of contained entities
						createForeignKey(r.higher, r.lower, false, false, SetNull)
					else
						// ?-*,?-+ - add nullable foreign key field to right side - deletion restricted
						createForeignKey(r.higher, r.lower, false, false, Restrict)
				case OrderedZeroOrMore =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> ZeroOrOne
					val linkFirst = r.higher.copy(cardinality = ZeroOrOne)
					handleSimpleRelationship(new Relationship(r.lower, linkFirst))
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = Endpoint(ZeroOrMore, r.higher.participant, Some(Names(new Name("prev") + r.higher.refName)), false)
					val linkNext = Endpoint(ZeroOrOne, r.higher.participant, Some(Names(new Name("next") + r.higher.refName)), false)
					handleSimpleRelationship(new Relationship(linkNext, linkPrev))
				case OrderedOneOrMore =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> One
					val linkFirst = r.higher.copy(cardinality = One)
					handleSimpleRelationship(new Relationship(r.lower, linkFirst))
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = Endpoint(ZeroOrMore, r.higher.participant, Some(Names(new Name("prev") + r.higher.refName)), false)
					val linkNext = Endpoint(ZeroOrOne, r.higher.participant, Some(Names(new Name("next") + r.higher.refName)), false)
					handleSimpleRelationship(new Relationship(linkNext, linkPrev))
			}
		case One =>
			r.higher.cardinality match {
				case One => 
					if (r.hasContainer)
						// 1>1 - add foreign key to container - restrict on deletion
						createForeignKey(r.container, r.contained, true, true, Restrict)
					else {
						// 1-1 - do not delete in either direction - requires map table
						// this should have limited use
						val table = createTableForSimpleRelationship(r)
						val leftField = createMapField(r.left, r.right.cardinality, Restrict)
						val rightField = createMapField(r.right, r.left.cardinality, Restrict)
						table.addField(rightField)
						table.addField(leftField)
						// both keys are unique
						table.uniqueKeys = List(rightField) :: List(leftField) :: table.uniqueKeys
					}
				case ZeroOrMore | OneOrMore =>
					if (r.lower.contained)
						// 1>*,1>+ - add foreign key to right side - restrict deletion
						createForeignKey(r.higher, r.lower, true, false, Restrict)
					else if (r.higher.contained)
						// 1<*,1<+ - could use a map table to prevent deletion of RHS
						// but normal practice to add foreign key to right side, cascade for deletion
						createForeignKey(r.higher, r.lower, true, false, Cascade)
					else
						// 1-*,1-+ - add foreign key field to right side
						createForeignKey(r.higher, r.lower, true, false, Restrict)
				case OrderedZeroOrMore =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> ZeroOrOne
					val linkFirst = r.higher.copy(cardinality = ZeroOrOne)
					handleSimpleRelationship(new Relationship(r.lower, linkFirst))
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = Endpoint(ZeroOrMore, r.higher.participant, Some(Names(new Name("prev") + r.higher.refName)), false)
					val linkNext = Endpoint(ZeroOrOne, r.higher.participant, Some(Names(new Name("next") + r.higher.refName)), false)
					handleSimpleRelationship(new Relationship(linkNext, linkPrev))
				case OrderedOneOrMore =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> One
					val linkFirst = r.higher.copy(cardinality = One)
					handleSimpleRelationship(new Relationship(r.lower, linkFirst))
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = Endpoint(ZeroOrMore, r.higher.participant, Some(Names(new Name("prev") + r.higher.refName)), false)
					val linkNext = Endpoint(ZeroOrOne, r.higher.participant, Some(Names(new Name("next") + r.higher.refName)), false)
					handleSimpleRelationship(new Relationship(linkNext, linkPrev))
			}
		case ZeroOrMore | OneOrMore =>
			r.higher.cardinality match {
				case ZeroOrMore | OneOrMore =>
					// *-*,*-+,+-+
					val table = createTableForSimpleRelationship(r)
					val containedField = createMapField(r.contained, r.container.cardinality, Restrict)
					val containerField = createMapField(r.container, r.contained.cardinality, if (r.hasContainer) Cascade else Restrict)
					table.addField(containerField)
					table.addField(containedField)
					// fields taken together are unique - put container field first, since it cascades
					// and deletion is more efficient if it is indexed.
					table.uniqueKeys = List(containerField, containedField) :: table.uniqueKeys
				case OrderedZeroOrMore =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> ZeroOrOne
					val linkFirst = r.higher.copy(cardinality = ZeroOrOne)
					handleSimpleRelationship(new Relationship(linkFirst, r.lower))
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = Endpoint(ZeroOrMore, r.higher.participant, Some(Names(new Name("prev") + r.higher.refName)), false)
					val linkNext = Endpoint(ZeroOrOne, r.higher.participant, Some(Names(new Name("next") + r.higher.refName)), false)
					handleSimpleRelationship(new Relationship(linkNext, linkPrev))
				case OrderedOneOrMore =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> One
					val linkFirst = r.higher.copy(cardinality = One)
					handleSimpleRelationship(new Relationship(linkFirst, r.lower))
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = Endpoint(ZeroOrMore, r.higher.participant, Some(Names(new Name("prev") + r.higher.refName)), false)
					val linkNext = Endpoint(ZeroOrOne, r.higher.participant, Some(Names(new Name("next") + r.higher.refName)), false)
					handleSimpleRelationship(new Relationship(linkNext, linkPrev))
			}
	}
	private val tables = new HashMap[Participant,Table]
		// Convert the E-R graph to a database schema, in several phases
		// 1. merge non-circular 1-1 relationships, Car 1-1 Owner can be transformed to CarOwner
		//    with original entity names prefixed to fields, e.g. fields car_name, owner_name
		//    We need to avoid loops, such as "prev: Node 1-1 next: Node"
		//    Alternatively, create a map table with each field being unique
		// 2. locate fields that are natural keys within a table (generally short unique, non-optional)
		// 3. For ?-1 relationships, where the ? side does not have a key, use the reference to the 1-side
		//    as the key (this may recurse if the 1-side does not have a key, but does have a ?-1 relationship
		//    (there shoudn't be any cycles, but need to check in case there's an error)
		//    BUG: this works IF the relationship is final - if the link will change, it breaks!
		// 4. For [?1]-[*+] relationships (and any remaining ?-1 relationships), get a key for the [?1]-side, 
		//    and add a field to the [*+]-side referencing the key (field name = table+field, field is not-null
		//    for 1-relation)
		// 5. For all other relationships (including 1-1 if not done in step 1.), create a map table (name of 
		//    table is "table[-fields] to table[-fields]", where fields specified only if not default (use plurals)
		//    e.g. Taxon *-* Synonym => TaxonToSynonym
		//         parent(s): Node *-* child(ren): Node => NodeParentsToNodeChildren
		// Best practice for normalisation is probably to represent all relationships not involving a 1-side
		// using step 5
	var name: Option[Name] = None
	def analyse(graph: ERGraph) = {
		name = graph.name
		// anything with properties or that is an endpoint in a relationship must be represented as a table
		var er: List[Participant] = Nil // build a list of participants that are both entity and relationship
		for (p <- graph.participants.values;
			if !(p.properties.isEmpty && p.relationships.isEmpty)
		) {
			val table = createTableForParticipant(p)
			p.properties foreach { property =>
				val field = Field(property.names.singular, MySQLDatabaseType(property.baseType), 
						notNull=(!property.optional), unique=property.unique, description=property.description)
				table.addField(field)
			}
			if (p.relationship.isDefined) er = p :: er
		}
		// now that we have the tables created for each endpoint, we can deal with the relationships
		// first, the ones that are already implemented as a table
		for (p <- er) { // our previously created sub-list
			val table = tables(p)
			val r = p.relationship.get
			val containedField = createMapField(r.contained, r.container.cardinality, Restrict)
			val containerField = createMapField(r.container, r.contained.cardinality, if (r.hasContainer) Cascade else Restrict)
			table.addField(containerField)
			table.addField(containedField)
			// put containerField first in index, if deletion cascades this index will speed it up
			// we don't need to do this if the container field is unique anyway
			r.contained.cardinality match {
				case ZeroOrOne | One => // the container reference will already be unique
				case _ => table.uniqueKeys = List(containerField, containedField) :: table.uniqueKeys
			}
		}
		// finally, we deal with the simple relationships - relationships that can often be dealt with using additional
		// fields rather than tables
		for (p <- graph.participants.values;
			if p.properties.isEmpty && p.relationships.isEmpty
		) {
			handleSimpleRelationship(p.relationship.get)
		}
	}
	def write(out: java.io.Writer) = {
		var seen = Set[Table]()
		def allReferencesSeen(table: Table): Boolean = {
			table.fields.forall(field => (field.references.isEmpty || field.references.get._1 == table || seen(field.references.get._1)))
		}
		val sb = new StringBuilder
		name.map(name => sb.append("/*\n** ").append(name.titleText).append("\n*/\n\n"))
		val a = collection.mutable.ArrayBuffer.concat(tables.values).sortWith(
				(t1:Table, t2:Table) => t1.name.lowerCase < t2.name.lowerCase)
		while (!a.isEmpty) {
			val idx = a.indexWhere(allReferencesSeen(_))
			if (idx < 0) {
				a.foreach{table => 
					println(table.name.lowerCase)
					for (field <- table.fields;
						refTable <- field.references) {
						println("  " + refTable._1.name.lowerCase)
					}
				}
			}
			val table = a.remove(idx)
			table.createClause(sb)
			sb.append("\n\n")
			seen = seen + table
		}
		out.write(sb.toString)
	}
}