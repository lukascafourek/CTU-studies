package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

@Data
@XmlType(name = "PE_REGKAND_ROW", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Candidate {

    @XmlElement(name = "CKAND", namespace = Constants.NAMESPACE)
    private int id;
    @XmlElement(name = "JMENO", namespace = Constants.NAMESPACE)
    private String forename;
    @XmlElement(name = "PRIJMENI", namespace = Constants.NAMESPACE)
    private String surname;
    @XmlElement(name = "VEK", namespace = Constants.NAMESPACE)
    private int age;

    public String getFullName() {
        return forename + " " + surname;
    }

}
