package cz.cvut.fel.omo.hw.functions.data.model;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;

@Data
@XmlRootElement(name = "UCAST", namespace = Constants.NAMESPACE)
@XmlAccessorType(XmlAccessType.FIELD)
public class VoterTurnout {

    @XmlAttribute(name = "KOLO")
    private int round;
    @XmlAttribute(name = "ZAPSANI_VOLICI")
    private int numberOfRegisteredVoters;
    @XmlAttribute(name = "VYDANE_OBALKY")
    private int numberOfIssuedVotingEnvelopes;
    @XmlAttribute(name = "ODEVZDANE_OBALKY")
    private int numberOfSubmittedVotingEnvelopes;
    @XmlAttribute(name = "PLATNE_HLASY")
    private int numberOfValidVotes;

}
