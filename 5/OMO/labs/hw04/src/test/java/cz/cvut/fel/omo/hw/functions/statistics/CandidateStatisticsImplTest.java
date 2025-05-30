package cz.cvut.fel.omo.hw.functions.statistics;

import cz.cvut.fel.omo.hw.functions.data.DataLoader;
import cz.cvut.fel.omo.hw.functions.data.DatasetEnum;
import cz.cvut.fel.omo.hw.functions.data.OfflineDataLoader;
import cz.cvut.fel.omo.hw.functions.data.model.Candidates;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

class CandidateStatisticsImplTest {

    private static CandidateStatistics candidateStatistics;
    private static Candidates candidates;

    @BeforeAll
    static void setup() throws ExecutionException, InterruptedException {
        DataLoader dataLoader = new OfflineDataLoader();
        CompletableFuture<Candidates> candidatesFuture = dataLoader.getCandidates(DatasetEnum.DATASET_2023_2ND_ROUND);
        candidateStatistics = new CandidateStatisticsImpl(candidatesFuture);
        candidates = candidatesFuture.get();
    }

    @Test
    @DisplayName("Test candidate average age.")
    void getAverageAge() {
        Assertions.assertTrue(candidateStatistics.getAverageAge() > 56.333333);
        Assertions.assertTrue(candidateStatistics.getAverageAge() < 56.4);
    }

    @Test
    @DisplayName("Test the oldest candidate name.")
    void getOldestCandidateName() {
        Assertions.assertEquals("Jaroslav Bašta", candidateStatistics.getOldestCandidateName());
    }

    @Test
    @DisplayName("Test candidate-age map.")
    void getCandidateAgeMap() {
        Map<String, Integer> candidateAgeMap = candidateStatistics.getCandidateAgeMap();
        candidates.getCandidatesList().forEach(candidate -> Assertions.assertEquals(candidate.getAge(), candidateAgeMap.get(candidate.getFullName())));
    }
}