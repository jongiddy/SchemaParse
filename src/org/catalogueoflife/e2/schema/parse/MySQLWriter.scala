package org.catalogueoflife.e2.schema.parse

class MySQLWriter extends RelationalWriter {
  def escapeIdentifier(word: String) = "`" + word + "`"
  def formatComment(text: String) = sqlStyle("comment '") + text + "'"
  def formatAutoIncrement() = sqlStyle("auto_increment")
  def formatTableSuffix() = sqlStyle(" engine=") + "InnoDB" + sqlStyle(" charset=") + "utf8"

	// types are suitable for use with MySQL 5.0.3 and above
	trait MySQLIntegralType extends DatabaseType
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
	trait MySQLTextualType extends DatabaseType
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
	case object MySQLDateType extends DatabaseType {
		override def toString = "date"
	}
	case object MySQLDateTimeType extends DatabaseType {
		override def toString = "datetime"
	}
	case object MySQLBoolType extends DatabaseType {
		override def toString = "boolean"
	}
	def DatabaseType(graphType: GraphType) : DatabaseType = {
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
				case _: BooleanType => MySQLBoolType
				case _ => throw new RuntimeException("cannot convert type")
			}
	}
  class Table(participant: Participant, name: Name) extends super.Table(participant, name) {
    def checkFieldAsKey(field: Field) = field.baseType match {
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
    def dedicatedKeyType = participant.maxInstances match {
        case None => MySQLIntUnsigned
        case Some(max) => DatabaseType(IntType(Some((BigInt(0), max))))
    }
  }
  def Table(participant: Participant, name: Name) = new Table(participant, name)
}