package securityAndValidation;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

import metamodel.AfAnnotation;
import metamodel.MetaModel;

import main.AFContext;

public class SecurityChecker {

	public static Boolean validate(AFContext afContext, metamodel.Field field) {
		Boolean annFlag = false;

		Iterator annotationIter = field.annotation.keySet().iterator();
		while (annotationIter.hasNext()) {
			String annotationKey = (String) annotationIter.next();
			AfAnnotation annotation = (AfAnnotation) field.annotation
					.get(annotationKey);
			if (annotation.name.equals("UiUserRoles")) {
				for (int i = 0; i < annotation.role.length; i++) {
					if (annotation.role[i].equals(afContext.userRole)) {
						return true;
					} else {
						annFlag = true;
					}
				}

				//
				/*
				 * if (annotation.message.equals(afContext.userRole)) { return
				 * true; } else { return false; }
				 */
			}
		}
		// if no security annotation
		if (annFlag == false) {
			afContext.instanceWithoutSecurity = true;
		} else {
			afContext.instanceWithoutSecurity = false;
		}
		return false;

	}

	public static Boolean validate(AFContext afContext, MetaModel metamodel) {
		Boolean annFlag = false;

		Iterator annotationIter = metamodel.annotation.keySet().iterator();
		while (annotationIter.hasNext()) {
			String annotationKey = (String) annotationIter.next();
			AfAnnotation annotation = (AfAnnotation) metamodel.annotation
					.get(annotationKey);
			if (annotation.name.equals("UiUserRoles")) {
				for (int i = 0; i < annotation.role.length; i++) {
					if (annotation.role[i].equals(afContext.userRole)) {
						return true;
					} else {
						annFlag = true;
					}
				}

				// for 1 user role (no array just String)
				/*
				 * if (annotation.message.equals(afContext.userRole)) { return
				 * true; } else { return false; }
				 */
			}
		}
		// if no security annotation
		if (annFlag == false) {
			afContext.instanceWithoutSecurity = true;
		} else {
			afContext.instanceWithoutSecurity = false;
		}

		return false;

	}

}