package cz.cvut.fel.omo.hw.functions.statistics;

import java.util.Map;

public interface CandidateStatistics {

    /**
     * Calculates the average age of the president candidate.
     *
     * @return average age
     */
    double getAverageAge();

    /**
     * Find the oldest president candidate.
     *
     * @return the full name of the oldest president candidate
     */
    String getOldestCandidateName();

    /**
     * Construct an age map of president candidates.
     *
     * @return Map where key is the full name (like "Petr Pavel") and the value is the age ot the candidate.
     */
    Map<String, Integer> getCandidateAgeMap();

}
