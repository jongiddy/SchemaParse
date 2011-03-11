package org.catalogueoflife.e2.schema.parse


/*
class SQLiteWriter(er: ERGraph) {

	trait SQLiteDatabaseType
	trait SQLiteIntegralType extends SQLiteDatabaseType
	case object SQLiteTinyIntUnsigned extends SQLiteIntegralType {
		override def toString = "tinyint unsigned"
	}
	case object SQLiteTinyIntSigned extends SQLiteIntegralType {
		override def toString = "tinyint"
	}
	case object SQLiteSmallIntUnsigned extends SQLiteIntegralType {
		override def toString = "smallint unsigned"
	}
	case object SQLiteSmallIntSigned extends SQLiteIntegralType {
		override def toString = "smallint"
	}
	case object SQLiteMediumIntUnsigned extends SQLiteIntegralType {
		override def toString = "mediumint unsigned"
	}
	case object SQLiteMediumIntSigned extends SQLiteIntegralType {
		override def toString = "mediumint"
	}
	case object SQLiteIntUnsigned extends SQLiteIntegralType {
		override def toString = "integer unsigned"
	}
	case object SQLiteIntSigned extends SQLiteIntegralType {
		override def toString = "integer"
	}
	case object SQLiteBigIntUnsigned extends SQLiteIntegralType {
		override def toString = "bigint unsigned"
	}
	case object SQLiteBigIntSigned extends SQLiteIntegralType {
		override def toString = "bigint"
	}
	case object SQLiteInteger extends SQLiteIntegralType {
		override def toString = "integer"
	}
	trait SQLiteTextualType extends SQLiteDatabaseType
	case class SQLiteCharType(size: Int) extends SQLiteTextualType {
		override def toString = "char(" + size + ")"
	}
	trait SQLiteVarCharOrTextType extends SQLiteTextualType
	case class SQLiteVarCharType(max: Int) extends SQLiteVarCharOrTextType {
		override def toString = "varchar(" + max + ")"
	}
	case object SQLiteTextType extends SQLiteVarCharOrTextType {
		override def toString = "text"
	}
	case object SQLiteMediumTextType extends SQLiteTextualType {
		override def toString = "mediumtext"
	}
	case object SQLiteLongTextType extends SQLiteTextualType {
		override def toString = "longtext"
	}
	case object SQLiteDateType extends SQLiteDatabaseType {
		override def toString = "date"
	}
	case object SQLiteDateTimeType extends SQLiteDatabaseType {
		override def toString = "datetime"
	}
	case object SQLiteBoolType extends SQLiteDatabaseType {
		override def toString = "boolean"
	}
	object SQLiteDatabaseType {
		def apply(graphType: GraphType) : SQLiteDatabaseType = {
			def twoto(power: Int) = BigInt(2) << (power - 1)
			graphType match {
				case IntType(Some((min, max))) if min >=0 && max < twoto(8) => SQLiteTinyIntUnsigned
				case IntType(Some((min, max))) if min >= -twoto(8-1) && max < twoto(8-1) => SQLiteTinyIntSigned
				case IntType(Some((min, max))) if min >=0 && max < twoto(16) => SQLiteSmallIntUnsigned
				case IntType(Some((min, max))) if min >= -twoto(16-1) && max < twoto(16-1) => SQLiteSmallIntSigned
				case IntType(Some((min, max))) if min >=0 && max < twoto(24) => SQLiteMediumIntUnsigned
				case IntType(Some((min, max))) if min >= -twoto(24-1) && max < twoto(24-1) => SQLiteMediumIntSigned
				case IntType(Some((min, max))) if min >=0 && max < twoto(32) => SQLiteIntUnsigned
				case IntType(Some((min, max))) if min >= -twoto(32-1) && max < twoto(32-1) => SQLiteIntSigned
				case IntType(Some((min, max))) if min >=0 && max < twoto(64) => SQLiteBigIntUnsigned
				case IntType(Some((min, max))) if min >= -twoto(64-1) && max < twoto(64-1) => SQLiteBigIntSigned
				case IntType(None) => SQLiteBigIntSigned //XXX
				case StringType(min, max) if min == max && max < twoto(8) => SQLiteCharType(max.intValue)
				// although varchar can be up to 2^16-1, that is also the entire row limit,
				// so assume we handle no more than 2^12
				// TODO - make it a SQLiteVarCharOrTextType, and then calculate row sizes before
				// deciding
				case StringType(min, max) if max < twoto(12) => SQLiteVarCharType(max.intValue)
				case StringType(min, max) if max < twoto(16) => SQLiteTextType
				case StringType(min, max) if max < twoto(24) => SQLiteMediumTextType
				case StringType(min, max) if max < twoto(32) => SQLiteLongTextType
				case IndeterminateStringType => SQLiteLongTextType
				case CalendarType => SQLiteDateType
				case ClockType => SQLiteDateTimeType
				case BooleanType => SQLiteBoolType
				case _ => throw new RuntimeException("cannot convert type")
			}
		}
	}

	trait SQLiteAction
	case object Restrict extends SQLiteAction {
		def asString = Restrict
	}
	case object Cascade extends SQLiteAction {
		def asString = Cascade
	}
	case object SetNull extends SQLiteAction {
		def asString = SetNull
	}
	
	var handled = Set[Relationship]();
	case class Field(name: Name, baseType: SQLiteDatabaseType, notNull: Boolean=false, unique: Boolean=false, autoIncrement: Boolean=false,
			references: Option[(Table,SQLiteAction)]=None, description: String="") {
		def originalName: Name = references match {
			case None => name
			case Some((table,_)) => table.key.get.originalName
		}
		def isContainer = references.map(_._2) match {
			case Some(Cascade) => true
			case Some(SetNull) => true
			case _ => false
		}
		def createClause(sb: StringBuilder, indent: String, isKey: Boolean=false) = {
			val fieldName = fieldNameStyle(name)
			sb.append(indent).append(escapeIdentifier(fieldName)).append(" " * math.max(21-fieldName.length, 1))
			sb.append(sqlTypeStyle(baseType.toString))
			if (isKey) sb.append(sqlStyle(" primary key"))
			else {
				if (unique) sb.append(sqlStyle(" unique"))
				if (notNull) sb.append(sqlStyle(" not null"))
			}
			if (autoIncrement) sb.append(sqlStyle(" /*auto_increment*/")) // XXX - auto_increment is automatic on integer primary keys
			references.map { ref =>
				val (table, onDeleteAction) = ref
				sb.append("\n").append(indent).append(" " * 24)
				sb.append(sqlStyle(" references ")).append(escapeIdentifier(tableNameStyle(table.name)))
				if (!table.hasDedicatedKey) sb.append(sqlStyle(" on update cascade"))
				if (onDeleteAction != Restrict) sb.append(sqlStyle(" on delete " + onDeleteAction))
			}
			// XXX - put comments in delimiters
			if (!description.isEmpty) {
				sb.append("\n").append(indent).append("      /* ").append(description).append(" */")
			}
		}
	}
	class Table(val name: Name, var fields: List[Field] = Nil, var key: Option[Field] = None) {
		var entity: Option[Entity] = None
		var hasDedicatedKey = false // indicates whether the key field was explicitly created to be a key (is not a data field)
		def getKey(): Field = getKey(Set[Table](this))
		private def getKey(seen: Set[Table]): Field = {
			// given a table, return a field suitable for use as a key
			// seen is a list of tables already seen during this operation (to avoid loops)
			key match {
				case Some(field) => field
				case None => {
					// Step 3: if table has a ?-1 contained relationship with another table, 
					// use the 1-table key as the key for this table
					for (r <- er.relationships;
						if r.left.cardinality == ZeroOrOne;
						if r.right.cardinality == One;
						if r.left.contained;
						if entityTables(r.left.entity) == this;
						if !seen(entityTables(r.right.entity))
					) {
						if (key isEmpty) {
							val rightTable = entityTables(r.right.entity);
							val rightKey = rightTable.getKey(seen + rightTable);
							val fieldName = r.right.refName + rightKey.originalName
							key = Some(Field(fieldName, rightKey.baseType, unique=true, notNull=true, references=Some((rightTable, Cascade))))
							hasDedicatedKey = rightTable.hasDedicatedKey
							handled = handled + r
						}
					}
					if (key isEmpty) {
						val keyType = SQLiteInteger // XXX - must be this type for key to auto increment
						key = Some(Field(new Name("id"), keyType, notNull=true, unique=true, autoIncrement=true))
						hasDedicatedKey = true
					}
					// At this point, we are certain that key is defined
					fields = key.get :: fields
					key.get
				}
			}
		}
		def doOrdering(fields: List[Field], acc: Array[List[Field]]) : Array[List[Field]] = {
			fields match {
				case Nil => acc
				case field :: rest if Some(field) == key => doOrdering(rest, acc)
				case field :: rest if field.unique && field.notNull => acc(0) = field :: acc(0) ; doOrdering(rest, acc)
				case field :: rest if field.isContainer => acc(1) = field :: acc(1) ; doOrdering(rest, acc)
				case field :: rest => acc(2) = field :: acc(2) ; doOrdering(rest, acc)
			}
		}
		def createClause(sb: StringBuilder) = {
			// the fields are created in reverse order, then relationships are added to the head
			// we reverse the list of fields in order to make the table define fields in order
			val ordered = doOrdering(fields, Array[List[Field]](Nil,Nil,Nil))
			sb.append(sqlStyle("create table ")).append(escapeIdentifier(tableNameStyle(name))).append(" (")
			var first = true
			if (key isDefined) {
				first = false
				sb.append("\n")
				key.get.createClause(sb, "  ", true)
			}
			for (fields <- ordered;
				field <- fields
			) {
				if (first) {
					first = false
					sb.append("\n")
				}
				else sb.append(",\n")
				field.createClause(sb, "  ")
			}
			sb.append("\n)").append(sqlStyle(" engine=")).append("InnoDB").append(sqlStyle(" charset=")).append("utf8;")
		}
	}
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

	override def toString : String = {
		var seen = Set[Table]()
		def allReferencesSeen(table: Table): Boolean = {
			table.fields.forall(field => (field.references.isEmpty || field.references.get._1 == table || seen(field.references.get._1)))
		}
		val sb = new StringBuilder
		val a = collection.mutable.ArrayBuffer.concat(entityTables.values, relationshipTables.values).sortWith(
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
		sb.toString
	}
	private lazy val entityTables = {
		val tables = new HashMap[Entity,Table]
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
		
		// Step 1 - we don't do this currently - no harm, as handled in step 5
		
		// Step 2 - create the tables and look for natural keys
		er.entities.foreach {kv =>
			val (name, entity) = kv
			val table = new Table(entity.names.singular)
			table.entity = Some(entity)
			tables(entity) = table
			entity.properties foreach { property =>
				val field = Field(property.names.singular, SQLiteDatabaseType(property.baseType), 
						notNull=(!property.optional), unique=property.unique, description=property.description)
				table.fields = field :: table.fields
				// is this field suitable to be a key?
				if (property.unique && ! property.optional) {
					field.baseType match {
						case _: SQLiteIntegralType =>
							// always use an integer, the latest one is earlier in the definition
							table.key = Some(field)
						case SQLiteCharType(size) if size <=8 =>
							// if no int key, use shortest String as long as length is <= 8
							table.key map { _.baseType } match {
								case None =>
									table.key = Some(field)
								case Some(SQLiteCharType(ksize)) if size <= ksize =>
									table.key = Some(field)
								case Some(SQLiteVarCharType(ksize)) if size <= ksize =>
									table.key = Some(field)
								case _ =>
							}
						case SQLiteVarCharType(size) if size <=8 =>
							// if no int key, use shortest String as long as length is <= 8
							table.key map { _.baseType } match {
								case None =>
									table.key = Some(field)
								case Some(SQLiteCharType(ksize)) if size <= ksize =>
									table.key = Some(field)
								case Some(SQLiteVarCharType(ksize)) if size <= ksize =>
									table.key = Some(field)
								case _ =>
							}
						case _ =>
					}
				}
			}
		}
		tables
	}
	private def createTableName(e: EntityRef): Name = {
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
		table.fields = containedField :: containerField :: table.fields
		table
	}
	// c is the cardinality of the related entity
	def createMapField(e: EntityRef, c: Cardinality, onDeleteAction: SQLiteAction) : Field = {
		val table = entityTables(e.entity)
		val key = table.getKey()
		Field(e.refName + key.originalName, key.baseType, notNull=true,
				unique=(c==ZeroOrOne || c==One), references=Some((table, onDeleteAction)))
	}
	def createForeignKey(contained: EntityRef, container: EntityRef, notNull: Boolean, unique: Boolean, onDeleteAction: SQLiteAction) {
		// add a foreign key to contained pointing to container
		val containedTable = entityTables(contained.entity)
		val containerTable = entityTables(container.entity)
		val containerKey = containerTable.getKey()
		val fieldName = container.refName + containerKey.originalName
		val field = Field(fieldName, containerKey.baseType, notNull, unique,
				references=Some((containerTable, onDeleteAction)))
		containedTable.fields = field :: containedTable.fields
	}
	def handleRelationship(r: Relationship, tables: HashMap[Relationship,Table]) : Unit = r.left.cardinality match {
		case ZeroOrOne =>
			r.right.cardinality match {
				case ZeroOrOne =>
					if (r.left.contained)
						// ?>? - add nullable foreign key to left side - set null on deletion
						createForeignKey(r.left, r.right, false, true, SetNull)
					else if (r.right.contained)
						// ?<? - add nullable foreign key to right side - set null on deletion
						createForeignKey(r.right, r.left, false, true, SetNull)
					else
						// ?-? - do not delete in either direction - requires map table
						tables(r) = createMapTable(r)
				case One =>
					if (r.left.contained)
						// ?>1 - add foreign key to left side - deletion of right entity deletes left entity
						createForeignKey(r.left, r.right, true, true, Cascade)
					else if (r.right.contained)
						// ?<1 - add nullable foreign key to right side - set null on deletion
						createForeignKey(r.right, r.left, true, true, SetNull)
					else
						// ?-1 - add foreign key to left side - deletion of right entity restricted
						createForeignKey(r.left, r.right, true, true, Restrict)
				case ZeroOrMore | OneOrMore =>
					if (r.left.contained)
						// ?>*,?>+ - add foreign key to right side - restrict deletion
						createForeignKey(r.right, r.left, false, false, Restrict)
					else if (r.right.contained)
						// ?<*,?<+ - add nullable foreign key to right side - set null on deletion
						createForeignKey(r.right, r.left, false, false, SetNull)
					else
						// ?-*,?-+ - add nullable foreign key field to right side - deletion restricted
						createForeignKey(r.right, r.left, false, false, Restrict)
				case OrderedZeroOrMore | _ : Ordered =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> One
					val linkFirst = new EntityRef(ZeroOrOne, r.right.entity, r.right.names, r.right.contained)
					handleRelationship(Relationship(r.left, linkFirst), tables)
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = new EntityRef(ZeroOrMore, r.right.entity, Some(Names(new Name("prev") + r.right.refName)), false)
					val linkNext = new EntityRef(ZeroOrOne, r.right.entity, Some(Names(new Name("next") + r.right.refName)), false)
					handleRelationship(Relationship(linkNext, linkPrev), tables)
			}
		case One =>
			r.right.cardinality match {
				case One => 
					if (r.left.contained)
						// 1>1 - add foreign key to left side - cascade on deletion
						createForeignKey(r.left, r.right, true, true, Cascade)
					else if (r.right.contained)
						// 1<1 - add foreign key to right side - cascade on deletion
						createForeignKey(r.right, r.left, true, true, Cascade)
					else
						// 1-1 - do not delete in either direction - requires map table
						// this should have limited use
						tables(r) = createMapTable(r)
				case ZeroOrMore | OneOrMore =>
					if (r.left.contained)
						// 1>*,1>+ - add foreign key to right side - restrict deletion
						createForeignKey(r.right, r.left, true, false, Restrict)
					else if (r.right.contained)
						// 1<*,1<+ - add foreign key to right side - cascade on deletion
						createForeignKey(r.right, r.left, true, false, Cascade)
					else
						// 1-*,1-+ - add foreign key field to right side - cascade on deletion
						createForeignKey(r.right, r.left, true, false, Restrict)
				case OrderedZeroOrMore | _ : Ordered =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> One
					val linkFirst = new EntityRef(ZeroOrOne, r.right.entity, r.right.names, r.right.contained)
					handleRelationship(Relationship(r.left, linkFirst), tables)
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = new EntityRef(ZeroOrMore, r.right.entity, Some(Names(new Name("prev") + r.right.refName)), false)
					val linkNext = new EntityRef(ZeroOrOne, r.right.entity, Some(Names(new Name("next") + r.right.refName)), false)
					handleRelationship(Relationship(linkNext, linkPrev), tables)
			}
		case ZeroOrMore | OneOrMore =>
			r.right.cardinality match {
				case ZeroOrMore | OneOrMore =>
					// *-*,*-+,+-+
					tables(r) = createMapTable(r)
				case OrderedZeroOrMore | _ : Ordered =>
					// first, create a relationship from LHS to a single RHS
					// same as old right, but with Ordered -> One
					val linkFirst = new EntityRef(ZeroOrOne, r.right.entity, r.right.names, r.right.contained)
					handleRelationship(Relationship(linkFirst, r.left), tables)
					// then, create a link in the RHS entity to the next RHS entity in sequence
					// each entity links optionally to a next entity, and is linked from any number of previous entries
					val linkPrev = new EntityRef(ZeroOrMore, r.right.entity, Some(Names(new Name("prev") + r.right.refName)), false)
					val linkNext = new EntityRef(ZeroOrOne, r.right.entity, Some(Names(new Name("next") + r.right.refName)), false)
					handleRelationship(Relationship(linkNext, linkPrev), tables)
			}
	}
	private lazy val relationshipTables = {
		val tables = new HashMap[Relationship,Table]()
		// Step 3 - force the evaluation of keys for the ?-side in ?-1 relationships. This will create any keys that
		// can be created using keys from another table.
		for (r <- er.relationships;
			if r.left.cardinality == ZeroOrOne;
			if r.right.cardinality == One
		) entityTables(r.left.entity).getKey()
		er.relationships.foreach { r =>
			if (!handled(r)) {
				handleRelationship(r, tables)
				handled = handled + r
			}
		}
		tables
	}
}*/