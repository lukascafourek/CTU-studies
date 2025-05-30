package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "KONTINENT", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Continent {

    @XmlAttribute(name = "ZKRATKA")
    private String abbr;
    @XmlAttribute(name = "NAZEV")
    private String name;
    @XmlElement(name = "STAT", namespace = Constants.NAMESPACE)
    private List<Country> countries;

}
