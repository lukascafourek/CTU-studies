package presentation;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import javax.naming.Context;

import main.AFContext;

import securityAndValidation.Email;
import securityAndValidation.Max;
import securityAndValidation.Min;
import securityAndValidation.NotNull;
import securityAndValidation.Password;
import securityAndValidation.UiUserRoles;

import metamodel.*;

//@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
public class Presentation {

    private AFContext afContext = null;
    private Context context = null;
    private Object instance = null;
    private MetaModel metamodel = null;
    private String layoutStyle = "linear"; // default
    private String type = null;
    private String name = null;
    private String valueOfAttribute = "";
    private String nameOfInstance = null;
    private ArrayList<Object> arraylistOfInstances;

    public Presentation(Context myContext, Object myinstance, ArrayList<Object> myArraylistOfInstances) {
        context = myContext;
        instance = myinstance;
        arraylistOfInstances = myArraylistOfInstances;
    }

    public Presentation(Context myContext, ArrayList<Object> myArraylistOfInstances) {
        context = myContext;
        arraylistOfInstances = myArraylistOfInstances;
    }

    public void setLayout(String layout) {
        this.layoutStyle = layout;
    }

    public void buildCache(ArrayList<Object> arraylistOfInstances, AFContext afContext) {
        this.afContext = afContext;
        if (afContext.cache == null) {
            afContext.cache = new HashMap<String, MetaModel>();
            for (Object object : arraylistOfInstances) {
                Class<?> clazz = object.getClass();
                Field[] fields = clazz.getDeclaredFields();
                MetaModel metamodel = new MetaModel();
                metamodel.name = clazz.getSimpleName();
                System.out.println("name: " + metamodel.name);
                System.out.println("-------------------");
                // ### getting annotations of instance ###
                Annotation[] annotations = clazz.getAnnotations();
                processAnnotations(annotations, metamodel.annotation);
                // ### getting fields and annotations of fields ###
                Field f = null;
                for (Field field : fields) {
                    try {
                        metamodel.Field myfield = new metamodel.Field();
                        myfield.name = field.getName();
                        f = clazz.getDeclaredField(myfield.name);
                        f.setAccessible(true);
                        if (field.getType().getName().equals("java.lang.String")) {
                            myfield.type = "String";
                            myfield.value = (String) f.get(object);

                        } else if (field.getType().getName().equals("int")) {
                            myfield.type = field.getType().getName();
                            myfield.value = Integer.toString((Integer) f.get(object));
                        } else {
                            myfield.type = field.getType().getName();
                            myfield.value = (String) f.get(object);
                        }

                        // ### getting annotations of field ###
                        annotations = field.getAnnotations();
                        processAnnotations(annotations, myfield.annotation);
                        metamodel.fields.put(myfield.name, myfield);
                        System.out.println("att name: " + myfield.name);
                        System.out.println("att type: " + myfield.type);
                        System.out.println("att value: " + myfield.value);
                    } catch (NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
                        e.printStackTrace();
                    }
                }
                //save metamodel to cache
                afContext.cache.put(metamodel.name, metamodel);
            }
        }
    }

    private void processAnnotations(Annotation[] annotations, Map<String, AfAnnotation> annotationMap) {
        for (Annotation annotation : annotations) {
            if (annotation instanceof Email myAnnotation) {
                AfAnnotation a = new AfAnnotation("Email", myAnnotation.message());
                annotationMap.put(a.name, a);
            }
            if (annotation instanceof NotNull myAnnotation) {
                AfAnnotation a = new AfAnnotation("NotNull", myAnnotation.message());
                annotationMap.put(a.name, a);
            }
            if (annotation instanceof UiUserRoles myAnnotation) {
                AfAnnotation a = new AfAnnotation("UiUserRoles", myAnnotation.role());
                annotationMap.put(a.name, a);
            }
            if (annotation instanceof Password) {
                AfAnnotation a = new AfAnnotation("Password");
                annotationMap.put(a.name, a);
            }
            if (annotation instanceof Min myAnnotation) {
                AfAnnotation a = new AfAnnotation("Min", myAnnotation.message(), myAnnotation.size());
                annotationMap.put(a.name, a);
            }
            if (annotation instanceof Max myAnnotation) {
                AfAnnotation a = new AfAnnotation("Max", myAnnotation.message(), myAnnotation.size());
                annotationMap.put(a.name, a);
            }
        }
    }
}
