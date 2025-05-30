package metamodel;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class Field {
	public String type;
	public String name;
	public String value;
	//set annotations
	public HashMap<String, AfAnnotation> annotation;

	public Field() {
		this.annotation = new HashMap<String, AfAnnotation>();
		this.value = "";
	}

	
}
