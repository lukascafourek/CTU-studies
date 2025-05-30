package securityAndValidation;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

import metamodel.AfAnnotation;
import metamodel.MetaModel;

import main.AFContext;

public class MaxChecker {


	public static List<String> validate(AFContext afContext) {
		List<String> errors = new ArrayList<String>();
		Boolean annotationForAllAtributes = false;

		Iterator iter = afContext.cache.keySet().iterator();
		while (iter.hasNext()) {
			String key = (String) iter.next();
			MetaModel metamodel = (MetaModel) afContext.cache.get(key);
			//System.out.println("checking instance ... " + metamodel.name);
			// annotations for all instance
			Iterator annotationIter = metamodel.annotation.keySet().iterator();
			while (annotationIter.hasNext()) {
				annotationForAllAtributes = false;
				String annotationKey = (String) annotationIter.next();
				AfAnnotation annotation = (AfAnnotation) metamodel.annotation
						.get(annotationKey);
				if (annotation.name.equals("Max")) {
					annotationForAllAtributes = true;
					// test every atribute if its notNull
					Iterator fieldAnnotationIter = metamodel.fields.keySet()
							.iterator();
					while (fieldAnnotationIter.hasNext()) {
						String fieldAnnotationKey = (String) fieldAnnotationIter
								.next();
						metamodel.Field field = (metamodel.Field) metamodel.fields
								.get(fieldAnnotationKey);
						if (field.value.length() > annotation.size) {
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
					//System.out.println("checking field ... " + field.name);
					// checking annotations of the field
					Iterator annotationInnerIter = field.annotation.keySet()
							.iterator();
					while (annotationInnerIter.hasNext()) {
						String annotationKey = (String) annotationInnerIter
								.next();
						AfAnnotation annotation = (AfAnnotation) field.annotation
								.get(annotationKey);
						if (annotation.name.equals("Max")) {
							System.out.println("velikost "+annotation.size);
							if (field.value.length() > annotation.size){
								errors.add(annotation.message);
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
			if (annotation.name.equals("Max") && (textParam.length() > annotation.size) ) {
				errors.add(annotation.message);
			}
		}
		
		return errors;
	}	
	

}