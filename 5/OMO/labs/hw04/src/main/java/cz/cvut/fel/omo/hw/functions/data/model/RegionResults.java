package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "VYSLEDKY_KRAJ", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class RegionResults {

    @XmlElement(name = "KRAJ", namespace = Constants.NAMESPACE)
    private List<Region> regions;

}
