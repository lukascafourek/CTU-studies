package cz.cvut.fel.omo.hw.functions.statistics;

import java.util.List;
import java.util.Map;

public interface ElectionStatistics {

    /**
     * Counts all valid votes in the dataset. In counts both region and abroad votes.
     *
     * @return total number of votes
     */
    int getTotalValidVotes();

    /**
     * Counts all invalid votes in the dataset. In counts both region and abroad votes.
     * Note: number of invalid votes = submitted envelopes - valid votes
     *
     * @return total number of invalid votes
     */
    int getTotalInvalidVotes();

    /**
     * Counts all registered voters in the dataset. In counts both region and abroad votes.
     *
     * @return total number of registered voters
     */
    int getTotalVoterCount();

    /**
     * Counts all issued envelopes in the dataset. In counts both region and abroad votes.
     *
     * @return total number of issued envelopes
     */
    int getTotalIssuedEnvelopes();

    /**
     * Counts all voter turnout percentage. In counts both region and abroad votes.
     * Note: voter turnout percent = issued envelopes / number of registered voters
     *
     * @return percentage of voter turnout
     */
    double getTotalVoterTurnout();


    /**
     * Counts all votes for all candidates.
     *
     * @return Map where key is the full name (like "Petr Pavel") and the value is count of all his/her votes.
     */
    Map<String, Integer> getCandidateVotesMap();

    /**
     * Counts percentage of votes for all candidates.
     *
     * @return Map where key is the full name (like "Petr Pavel") and the value is percentage of all his/her votes.
     */
    Map<String, Double> getCandidateVotesPercentMap();

    /**
     * Returns formatted string of all candidates in format "Candidate 1 full name (percentage result), ...)". Percentage result is rounded to two decimal places.
     * Example string: "Petr Pavel (58,33%), Andrej Babiš (41,67%)"
     *
     * @return Formatted string with all candidates, sorted by votes count.
     */
    String getCandidatesByVotesDesc();
}
