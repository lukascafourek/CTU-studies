package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "OKRES", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class District {

    @XmlAttribute(name = "NAZ_OKRES")
    private String name;
    @XmlElement(name = "OBEC", namespace = Constants.NAMESPACE)
    private List<City> cities;

}
