package org.catalogueoflife.e2.schema.parse

import java.io.Writer

class SQLiteWriter extends RelationalWriter {
  def escapeIdentifier(word: String) = "`" + word + "`"
  def formatComment(text: String) = "/* " + text + " */"
  def formatAutoIncrement() = "/*" + sqlStyle("autoincrement") + "*/"


  // SQLite type system is not really thi extensive, but these types
  // are supported for compatibility with MySQL - so allows future
  // expansion if SQLite takes advantage of the finer details
	trait SQLiteIntegralType extends DatabaseType
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
  // SQLite automatically increments any integer primary key
  // use int instead of integer to avoid automatic auto_increment
	case object SQLiteIntUnsigned extends SQLiteIntegralType {
		override def toString = "int unsigned"
	}
	case object SQLiteIntSigned extends SQLiteIntegralType {
		override def toString = "int"
	}
  case object SQLiteAutoIncrementInteger extends SQLiteIntegralType {
		override def toString = "integer"
	}
	case object SQLiteBigIntUnsigned extends SQLiteIntegralType {
		override def toString = "bigint unsigned"
	}
	case object SQLiteBigIntSigned extends SQLiteIntegralType {
		override def toString = "bigint"
	}
	trait SQLiteTextualType extends DatabaseType
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
	case object SQLiteDateType extends DatabaseType {
		override def toString = "date"
	}
	case object SQLiteDateTimeType extends DatabaseType {
		override def toString = "datetime"
	}
	case object SQLiteBoolType extends DatabaseType {
		override def toString = "boolean"
	}
	def DatabaseType(graphType: GraphType) : DatabaseType = {
			def twoto(power: Int) = BigInt(2) << (power - 1)
			graphType match {
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(8) => SQLiteTinyIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(8-1) && max < twoto(8-1) => SQLiteTinyIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(16) => SQLiteSmallIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(16-1) && max < twoto(16-1) => SQLiteSmallIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(24) => SQLiteMediumIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(24-1) && max < twoto(24-1) => SQLiteMediumIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(32) => SQLiteIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(32-1) && max < twoto(32-1) => SQLiteIntSigned
				case IntType(Some((min, max)), _) if min >=0 && max < twoto(64) => SQLiteBigIntUnsigned
				case IntType(Some((min, max)), _) if min >= -twoto(64-1) && max < twoto(64-1) => SQLiteBigIntSigned
				case IntType(None, _) => SQLiteBigIntSigned
				case SizedStringType(min, max) if min == max && max < twoto(8) => SQLiteCharType(max.intValue)
				case SizedStringType(min, max) if max < twoto(12) => SQLiteVarCharType(max.intValue)
				case SizedStringType(min, max) if max < twoto(16) => SQLiteTextType
				case SizedStringType(min, max) if max < twoto(24) => SQLiteMediumTextType
				case SizedStringType(min, max) if max < twoto(32) => SQLiteLongTextType
				case UnsizedStringType => SQLiteLongTextType
				case CalendarType => SQLiteDateType
				case ClockType => SQLiteDateTimeType
				case _: BooleanType => SQLiteBoolType
				case _ => throw new RuntimeException("cannot convert type")
			}
	}
  class Table(participant: Participant, name: Name) extends super.Table(participant, name) {
    def checkFieldAsKey(field: Field) = field.baseType match {
      case _: SQLiteIntegralType =>
        // always use an integer over a string
        key map { _.baseType } match {
          case None | Some(_ : SQLiteCharType) | Some(_ : SQLiteVarCharType)=>
            setKey(field)
          case _ =>
        }
      case SQLiteCharType(size) if size <=8 =>
        // if no int key, use shortest String as long as length is <= 8
        key map { _.baseType } match {
          case None =>
            setKey(field)
          case List(SQLiteCharType(ksize)) if size <= ksize =>
            setKey(field)
          case List(SQLiteVarCharType(ksize)) if size <= ksize =>
            setKey(field)
          case _ =>
        }
      case SQLiteVarCharType(size) if size <=8 =>
        // if no int key, use shortest String as long as length is <= 8
        key map { _.baseType } match {
          case None =>
            setKey(field)
          case List(SQLiteCharType(ksize)) if size <= ksize =>
            setKey(field)
          case List(SQLiteVarCharType(ksize)) if size <= ksize =>
            setKey(field)
          case _ =>
        }
      case _ =>
    }
    def dedicatedKeyType = SQLiteAutoIncrementInteger
  }
  def Table(participant: Participant, name: Name) = new Table(participant, name)
}