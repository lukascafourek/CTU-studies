package cz.cvut.fel.omo.hw.functions.utils;

import cz.cvut.fel.omo.hw.functions.data.model.Vote;
import cz.cvut.fel.omo.hw.functions.data.model.VoterTurnout;

import java.util.List;

public interface ElectionsUtils {

    /**
     * Finds and returns all {@link Vote} objects from the dataset.
     * It takes votes from region results and also from abroad results.
     *
     * @return all {@link Vote} objects from the dataset
     */
    List<Vote> getAllVotes();

    /**
     * Finds and returns all {@link VoterTurnout} objects from the dataset.
     * It takes voter turnouts from region results and also from abroad results.
     *
     * @return all {@link VoterTurnout} objects from the dataset
     */
    List<VoterTurnout> getAllVoterTurnouts();

}

