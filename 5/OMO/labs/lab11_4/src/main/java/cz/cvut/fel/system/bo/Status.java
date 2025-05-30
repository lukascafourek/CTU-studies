/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

/**
 *
 * @author Jirka
 */
public enum Status {

    entrance_notGranted("Pred korektúrou"), entrance_granted("Pridelený korektorvi"), checked("Skontrolovaný");
    private final String label;

    private Status(String label) {
        this.label = label;
    }

    public String getLabel() {
        return this.label;
    }
}