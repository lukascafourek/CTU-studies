package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "ZAHRANICI", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Abroad {

    @XmlElement(name = "KONTINENT", namespace = Constants.NAMESPACE)
    private List<Continent> continents;

}
