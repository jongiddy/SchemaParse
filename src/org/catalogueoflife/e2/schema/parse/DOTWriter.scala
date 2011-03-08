package org.catalogueoflife.e2.schema.parse

class DOTWriter {

	val entityNameStyle = (name: Name) => name.titleText
	val propertyNameStyle = (name: Name) => name.lowerCase
	var er: Option[ERGraph] = None
	
	def analyse(graph: ERGraph) {
		er = Some(graph)
	}
	def write(out: java.io.Writer) {
		val graph = er.get
		val sb = new StringBuilder
		var indentLevel=0
		def indent = sb.append(" " * 3 * indentLevel)
		indent.append("graph ").append(graph.name.map(_.pascalCase).getOrElse("ER")).append(" {\n")
		indentLevel += 1
		graph.name.map(name => indent.append("label = \"").append(name.titleText).append("\";\n"))
		indent.append("fontsize=20;\n")
		indent.append("overlap=\"false\";\n")
		indent.append("splines=\"true\";\n")
		indent.append("concentrate=\"false\";\n")
		indent.append("orientation=land;\n\n")
		indent.append("node [shape=record];\n")
		indent.append("rankdir=\"LR\";\n\n")
		for (e <- graph.participants.values;
			if !e.relationships.isEmpty
		) {
			val entityName = e.names.get.singular.pascalCase
			indent.append("\"").append(e.names.get.singular.pascalCase).append("\" [\n")
			indentLevel +=1
			indent.append("label=\"").append(entityNameStyle(e.names.get.singular))
			e.properties foreach {p =>
				sb.append("|")
				if (p.optional) sb.append("? ")
				else if (p.unique) sb.append("= ")
				sb.append(propertyNameStyle(p.names.singular))
				sb.append(": ").append(p.baseType)
			}
			sb.append("\"\n")
			indentLevel -= 1
			indent.append("];\n")
		}
		for (p <- graph.participants.values;
			if !p.relationships.isEmpty;
			if p.relationship.isDefined
		) {
			val r = p.relationship.get
			indent.append("\"").append(r.left.participant.names.get.singular.pascalCase)
			sb.append("\" -- ").append("\"").append(p.names.get.singular.pascalCase)
			sb.append("\" [taillabel=\"1")
			r.left.names.map(names => sb.append(" ").append(propertyNameStyle(names.singular)))
			sb.append("\",headlabel=\"").append(r.left.cardinality)
			sb.append("\"];\n")
			indent.append("\"").append(p.names.get.singular.pascalCase)
			sb.append("\" -- ").append("\"").append(r.right.participant.names.get.singular.pascalCase)
			sb.append("\" [taillabel=\"").append(r.right.cardinality)
			r.left.names.map(names => sb.append(" ").append(propertyNameStyle(names.singular)))
			sb.append("\",headlabel=\"1\"];\n")
		}
		for (p <- graph.participants.values;
			if p.relationships.isEmpty
		) {
			val r = p.relationship.get
			indent.append("\"").append(r.left.participant.names.get.singular.pascalCase)
			sb.append("\" -- ").append("\"").append(r.right.participant.names.get.singular.pascalCase)
			sb.append("\" [taillabel=\"").append(r.left.cardinality)
			r.left.names.map(names => sb.append(" ").append(propertyNameStyle(names.singular)))
			sb.append("\",headlabel=\"").append(r.right.cardinality)
			r.right.names.map(names => sb.append(" ").append(propertyNameStyle(names.singular)))
			sb.append("\"];\n")
		}
		indentLevel -= 1
		indent.append("}\n")
		out.write(sb.toString)
	}
}
