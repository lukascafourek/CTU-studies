package main;

import java.util.ArrayList;
import java.util.Date;
import metamodel.MetaModel;
import model.*;
import presentation.Presentation;

public class Main {
    private static AFContext afContext;

    public static void main(String[] args) {
        long lStartTime = new Date().getTime();

        afContext = new AFContext();
        afContext.userRole = "ROLE_ADMIN"; //adding user a role: ROLE_ADMIN, ROLE_USER. ROLE_UNAUTHORIZED

        ArrayList arraylistOfInstances = new ArrayList();
        arraylistOfInstances.add(new ClassExample());
        arraylistOfInstances.add(new ClassExample2());
        try {
            Presentation p = new Presentation(null, arraylistOfInstances);//context null, jinak se jedna o kontext například v android applikaci
            p.setLayout("linear2"); //relative
            p.buildCache(arraylistOfInstances, afContext);
        } catch (Exception ex) {
            System.out.println("fail AF");
            ex.printStackTrace();
        }
        //end time
        long lEndTime = new Date().getTime();
        long difference = lEndTime - lStartTime;
        System.out.println("Elapsed milliseconds: " + difference);
        System.out.println("end AF");
    }
}
