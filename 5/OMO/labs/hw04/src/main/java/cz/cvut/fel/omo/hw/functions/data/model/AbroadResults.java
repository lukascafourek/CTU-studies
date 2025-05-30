package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;

@Data
@XmlRootElement(name = "VYSLEDKY_ZAHRANICI", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class AbroadResults {

    @XmlElement(name = "ZAHRANICI", namespace = Constants.NAMESPACE)
    private Abroad abroad;

}
