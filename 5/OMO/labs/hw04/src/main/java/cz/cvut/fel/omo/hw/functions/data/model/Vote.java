package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;

@Data
@XmlRootElement(name = "HODN_KAND", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Vote {

    @XmlAttribute(name = "PORADOVE_CISLO")
    private int candidateId;
    @XmlAttribute(name = "HLASY")
    private int votes;

}
