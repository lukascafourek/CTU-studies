package cz.cvut.fel.omo.hw.functions.statistics;

import java.util.List;
import java.util.Map;

public interface NationalStatistics {

    String getNameOfCityWithTheLowestVoterTurnout();

    String getNameOfCityWithTheHighestNonValidVotesRatio();

    /**
     * Find and return top 10 cities where candidate won given by candidate id.
     * Note: Draw also counts.
     *
     * @param candidateId id of presidential candidate
     * @return top 10 cities where candidate won (or draw), ordered by number of votes in descending order
     */
    List<String> getTop10CitiesWhereCandidateWonOrderedByNumberOfVotesDesc(int candidateId);

    Map<String, String> getRegionWinnerMap();

}
