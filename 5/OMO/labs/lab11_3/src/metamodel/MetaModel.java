package metamodel;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class MetaModel{
	public String name;
	public HashMap<String, Field> fields;
	public HashMap<String, AfAnnotation> annotation;
	
	public MetaModel() {
		this.name = "";
		this.fields = new HashMap<String, Field>();
		this.annotation = new HashMap<String, AfAnnotation>();
	}

}
