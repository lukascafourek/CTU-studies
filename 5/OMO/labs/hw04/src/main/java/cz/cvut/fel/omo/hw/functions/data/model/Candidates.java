package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;


@Data
@XmlRootElement(name = "PE_REGKAND", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class Candidates {

    @XmlElement(name = "PE_REGKAND_ROW", namespace = Constants.NAMESPACE)
    private List<Candidate> candidatesList = new ArrayList<>();

}
