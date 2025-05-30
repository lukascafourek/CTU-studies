package metamodel;

import java.util.HashSet;
import java.util.Set;

public class AfAnnotation {
	public String name;
	public String message;
	public String [] role;
	public int size;
	
	public AfAnnotation(String name) {
		this.name = name;
	}
	
	public AfAnnotation(String name,  String message) {
		this.name = name;
		this.message = message;
	}
	
	public AfAnnotation(String name,  String [] role) {
		this.name = name;
		this.role = role;
	}
	
	public AfAnnotation(String name, String message, int size) {
		this.name = name;
		this.message = message;
		this.size = size;
	}
}
