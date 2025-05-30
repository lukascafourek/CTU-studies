package securityAndValidation;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import metamodel.AfAnnotation;
import metamodel.MetaModel;
import main.AFContext;

public class EmailChecker {

	private static Pattern pattern;
	private static Matcher matcher;
 
	private static final String EMAIL_PATTERN = 
		"^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@"
		+ "[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$";
	
	
	public static List<String> validate(Object obj) {
		List<String> errors = new ArrayList<String>();
		Field[] fields = obj.getClass().getDeclaredFields();

		for (int i = 0; i < fields.length; i++) {
			fields[i].setAccessible(true);
			Email annotations = (Email) fields[i].getAnnotation(Email.class);
			
			if (annotations != null) {
				try {
					pattern = Pattern.compile(EMAIL_PATTERN);
					matcher = pattern.matcher(fields[i].get(obj).toString());
					if (!matcher.matches()) {
						errors.add(((Email) annotations).message());
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		return errors;
	}
	
	public static List<String> validate(AFContext afContext) {
		List<String> errors = new ArrayList<String>();
		Boolean annotationForAllAtributes = false;

		Iterator iter = afContext.cache.keySet().iterator();
		while (iter.hasNext()) {
			String key = (String) iter.next();
			MetaModel metamodel = (MetaModel) afContext.cache.get(key);
			//System.out.println("checking instance (email) ... " + metamodel.name);
			// annotations for all instance
			Iterator annotationIter = metamodel.annotation.keySet().iterator();
			while (annotationIter.hasNext()) {
				annotationForAllAtributes = false;
				String annotationKey = (String) annotationIter.next();
				AfAnnotation annotation = (AfAnnotation) metamodel.annotation
						.get(annotationKey);
				if (annotation.name.equals("Email")) {
					annotationForAllAtributes = true;
					// test every atribute if its notNull
					Iterator fieldAnnotationIter = metamodel.fields.keySet()
							.iterator();
					while (fieldAnnotationIter.hasNext()) {
						String fieldAnnotationKey = (String) fieldAnnotationIter
								.next();
						metamodel.Field field = (metamodel.Field) metamodel.fields
								.get(fieldAnnotationKey);
						if (field.value.equals("") || field.value.equals(null)) {
							errors.add(annotation.message);
						}
					}
				}
			}

			// checking fields
			if (annotationForAllAtributes == false) {
				Iterator fieldIter = metamodel.fields.keySet().iterator();
				while (fieldIter.hasNext()) {
					String fieldKey = (String) fieldIter.next();
					metamodel.Field field = (metamodel.Field) metamodel.fields
							.get(fieldKey);
					// checking annotations of the field
					Iterator annotationInnerIter = field.annotation.keySet()
							.iterator();
					while (annotationInnerIter.hasNext()) {
						String annotationKey = (String) annotationInnerIter
								.next();
						AfAnnotation annotation = (AfAnnotation) field.annotation
								.get(annotationKey);
						//System.out.println("checking field ... " + field.name + field.value);
						if (annotation.name.equals("Email")) {
							try {
								pattern = Pattern.compile(EMAIL_PATTERN);
								matcher = pattern.matcher(field.value);
								if (!matcher.matches()) {
									errors.add(annotation.message);
								}
							} catch (Exception e) {
								e.printStackTrace();
							}
							
						}
					}
				}
			}
		}

		return errors;
	}
	
	public static List<String> validate(metamodel.Field field, String textParam) {
		List<String> errors = new ArrayList<String>();
		
		Iterator annotationInnerIter = field.annotation.keySet()
				.iterator();
		while (annotationInnerIter.hasNext()) {
			String annotationKey = (String) annotationInnerIter
					.next();
			AfAnnotation annotation = (AfAnnotation) field.annotation
					.get(annotationKey);
			if (annotation.name.equals("Email")) {
				try {
					pattern = Pattern.compile(EMAIL_PATTERN);
					matcher = pattern.matcher(field.value);
					if (!matcher.matches()) {
						errors.add(annotation.message);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		
		return errors;
	}
	
	
}