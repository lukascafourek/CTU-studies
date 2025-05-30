package cz.cvut.fel.omo.hw.functions.statistics;

import cz.cvut.fel.omo.hw.functions.data.DataLoader;
import cz.cvut.fel.omo.hw.functions.data.DatasetEnum;
import cz.cvut.fel.omo.hw.functions.data.OfflineDataLoader;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Map;

class ElectionStatisticsImplTest {

    private static ElectionStatistics electionStatistics;

    @BeforeAll
    static void setup() {
        DataLoader dataLoader = new OfflineDataLoader();
        electionStatistics = new ElectionStatisticsImpl(dataLoader.getRegionResultsList(DatasetEnum.DATASET_2023_2ND_ROUND), dataLoader.getAbroadResults(DatasetEnum.DATASET_2023_2ND_ROUND), dataLoader.getCandidates(DatasetEnum.DATASET_2023_2ND_ROUND));
        // test results source: https://www.volby.cz/pls/prez2023nss/pe2?xjazyk=CZ
    }

    @Test
    @DisplayName("Test total valid votes.")
    void getTotalValidVotes() {
        Assertions.assertEquals(5759199, electionStatistics.getTotalValidVotes());
    }

    @Test
    @DisplayName("Test total invalid votes.")
    void getTotalInvalidVotes() {
        Assertions.assertEquals(5787540 - 5759199, electionStatistics.getTotalInvalidVotes());
    }

    @Test
    @DisplayName("Test total voter count.")
    void getTotalVoterCount() {
        Assertions.assertEquals(8242566, electionStatistics.getTotalVoterCount());
    }

    @Test
    @DisplayName("Test total issued envelopes.")
    void getTotalIssuedEnvelopes() {
        Assertions.assertEquals(5790001, electionStatistics.getTotalIssuedEnvelopes());
    }

    @Test
    @DisplayName("Test total voter turnout.")
    void getTotalVoterTurnout() {
        Assertions.assertTrue(electionStatistics.getTotalVoterTurnout() > 70.245);
        Assertions.assertTrue(electionStatistics.getTotalVoterTurnout() <= 70.254);
    }

    @Test
    @DisplayName("Test candidate-votes count map.")
    void getCandidateVotesMap() {
        Map<String, Integer> candidateVotesMap = electionStatistics.getCandidateVotesMap();
        Assertions.assertEquals(3359301, candidateVotesMap.get("Petr Pavel"));
        Assertions.assertEquals(2399898, candidateVotesMap.get("Andrej Babiš"));
    }

    @Test
    @DisplayName("Test candidate-votes percent map.")
    void getCandidateVotesPercentMap() {
        Map<String, Double> candidateVotesMap = electionStatistics.getCandidateVotesPercentMap();
        Assertions.assertTrue(candidateVotesMap.get("Petr Pavel") >= 58.32);
        Assertions.assertTrue(candidateVotesMap.get("Petr Pavel") < 58.33);
        Assertions.assertTrue(candidateVotesMap.get("Andrej Babiš") >= 41.67);
        Assertions.assertTrue(candidateVotesMap.get("Andrej Babiš") < 41.68);
    }

    @Test
    @DisplayName("Test candidates by votes string.")
    void getCandidatesByVotesDesc() {
        Assertions.assertEquals(String.format("Petr Pavel (%.2f%%), Andrej Babiš (%.2f%%)", 58.33, 41.67), electionStatistics.getCandidatesByVotesDesc());
    }
}