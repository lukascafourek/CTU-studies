package cz.cvut.fel.omo.hw.functions.statistics;

import cz.cvut.fel.omo.hw.functions.data.DataLoader;
import cz.cvut.fel.omo.hw.functions.data.DatasetEnum;
import cz.cvut.fel.omo.hw.functions.data.OfflineDataLoader;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

class NationalStatisticsImplTest {

    private static NationalStatistics nationalStatistics;

    @BeforeAll
    static void setup() {
        DataLoader dataLoader = new OfflineDataLoader();
        nationalStatistics = new NationalStatisticsImpl(dataLoader.getRegionResultsList(DatasetEnum.DATASET_2023_2ND_ROUND), dataLoader.getCandidates(DatasetEnum.DATASET_2023_2ND_ROUND));
        // test results source: https://www.volby.cz/pls/prez2023nss/pe2?xjazyk=CZ
    }


    @Test
    void getNameOfCityWithTheLowestVoterTurnout() {
        Assertions.assertEquals("Zběšičky", nationalStatistics.getNameOfCityWithTheLowestVoterTurnout());
    }

    @Test
    void getNameOfCityWithTheHighestNonValidVotesRatio() {
        Assertions.assertEquals("Bělá", nationalStatistics.getNameOfCityWithTheHighestNonValidVotesRatio());
    }

    @Test
    void getTop10CitiesWhereCandidateWonOrderedByNumberOfVotesDesc() {
        List<String> expected = List.of("Praha", "Brno", "Plzeň", "Olomouc", "Hradec Králové", "Liberec", "České Budějovice", "Pardubice", "Zlín", "Ústí nad Labem");
        List<String> actual = nationalStatistics.getTop10CitiesWhereCandidateWonOrderedByNumberOfVotesDesc(4);
        Assertions.assertIterableEquals(expected, actual);
    }

    @Test
    void getRegionWinnerMap() {
        Map<String, String> regionWinnerMap = nationalStatistics.getRegionWinnerMap();
        Assertions.assertEquals("Petr Pavel", regionWinnerMap.get("Olomoucký kraj"));
        Assertions.assertEquals("Andrej Babiš", regionWinnerMap.get("Ústecký kraj"));
    }
}