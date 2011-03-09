package main

import java.io.FileReader
import org.catalogueoflife.e2.schema.parse.SchemaParser
import org.catalogueoflife.e2.schema.parse.ERGraph
import org.catalogueoflife.e2.schema.parse.MySQLWriter
//import org.catalogueoflife.e2.schema.parse.SQLiteWriter
import org.catalogueoflife.e2.schema.parse.DOTWriter
import org.catalogueoflife.e2.schema.parse.XSDWriter

object Main {
	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
		val p = new java.io.PrintWriter(f)
		try { op(p) } finally { p.close() }
	}

	def main(args : Array[String]) {
		val filename = if (args.length > 0) args(0) else "/home/scmjpg/schema/MultiSchema.txt"
		val lastSlash = filename.lastIndexOf("/")
    val destDir = if (args.length > 1) args(1) else filename.slice(0, lastSlash)
    val lastDot = filename.lastIndexOf(".")
		val stub = destDir + (if (lastDot > 0 && lastDot > lastSlash + 1) filename.slice(lastSlash, lastDot)
      else filename.slice(lastSlash, filename.length))
		val mysqlFile = stub + "-MySQL5.sql"
		val sqliteFile = stub + "-SQLite.sql"
		val dotFile = stub + ".dot"
		val xsdFile = stub + ".xsd"
		val reader = new FileReader(filename)
		val er = new ERGraph
		val parser = new SchemaParser(er)
		parser.parseAll(parser.graph, reader) match {
			case parser.Success(graph, _) => {
				er.print()
				er.validate()
			 	val mysql = new MySQLWriter()
			 	mysql.analyse(er)
			 	printToFile(new java.io.File(mysqlFile)) { writer => mysql.write(writer) }
			 	val dot = new DOTWriter()
			 	dot.analyse(er)
			 	printToFile(new java.io.File(dotFile)) { writer => dot.write(writer) }
			 	val xsd = new XSDWriter()
			 	xsd.analyse(er)
			 	printToFile(new java.io.File(xsdFile)) { writer => xsd.write(writer) }
				/*
			 	printToFile(new java.io.File(dotFile)) { file =>
			 	 	file.println(new DOTWriter(er))
			 	}
			 	printToFile(new java.io.File(xsdFile)) { file =>
			 		val x = new XSDWriter
			 		x.process(er)
			 	 	file.println(x)
			 	}
			 	*/
			}
			case p@parser.Failure(msg, next) => println("Fail: " + p);
			case p@parser.Error(msg, next) => println("Error: " + p);
		}
	}
}
