package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "KRAJ", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Region {

    @XmlAttribute(name = "NAZ_KRAJ")
    private String name;
    @XmlElement(name = "OKRES", namespace = Constants.NAMESPACE)
    private List<District> districts;

}
