package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

import java.util.List;

@Data
@XmlRootElement(name = "STAT", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Country {

    @XmlAttribute(name = "ZKRATKA")
    private String abbr;
    @XmlAttribute(name = "NAZEV")
    private String name;
    @XmlElement(name = "HODN_KAND", namespace = Constants.NAMESPACE)
    private List<Vote> votes;
    @XmlElement(name = "UCAST", namespace = Constants.NAMESPACE)
    private VoterTurnout voterTurnout;
}
