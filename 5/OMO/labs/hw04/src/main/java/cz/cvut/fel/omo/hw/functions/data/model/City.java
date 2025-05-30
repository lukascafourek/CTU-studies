package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Cleanup;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "OBEC", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class City {

    @XmlAttribute(name = "NAZ_OBEC")
    private String name;
    @XmlAttribute(name = "TYP_OBEC")
    private String cityType;
    @XmlElement(name = "HODN_KAND", namespace = Constants.NAMESPACE)
    private List<Vote> votes;
    @XmlElement(name = "UCAST", namespace = Constants.NAMESPACE)
    private VoterTurnout voterTurnout;

}
