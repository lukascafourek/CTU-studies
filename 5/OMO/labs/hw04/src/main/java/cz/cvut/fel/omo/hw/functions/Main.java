package cz.cvut.fel.omo.hw.functions;

import cz.cvut.fel.omo.hw.functions.data.DataLoader;
import cz.cvut.fel.omo.hw.functions.data.DatasetEnum;
import cz.cvut.fel.omo.hw.functions.data.OfflineDataLoader;
import cz.cvut.fel.omo.hw.functions.statistics.*;

public class Main {

    private static final String DELIMITER = "--------------------------------------";

    public static void main(String[] args) {
//        DatasetEnum datasetEnum = DatasetEnum.DATASET_2018_1ST_ROUND;
//        DatasetEnum datasetEnum = DatasetEnum.DATASET_2018_2ND_ROUND;
//        DatasetEnum datasetEnum = DatasetEnum.DATASET_2023_1ST_ROUND;
        DatasetEnum datasetEnum = DatasetEnum.DATASET_2023_2ND_ROUND;

        DataLoader dataLoader = new OfflineDataLoader();
        CandidateStatistics candidateStatistics = new CandidateStatisticsImpl(dataLoader.getCandidates(datasetEnum));

        System.out.println(DELIMITER);
        System.out.println("CANDIDATE STATISTICS");
        System.out.println(DELIMITER);

        System.out.println();

        System.out.println("Candidate age map: " + candidateStatistics.getCandidateAgeMap());
        System.out.println(DELIMITER);
        System.out.println("Average candidate age: " + candidateStatistics.getAverageAge());
        System.out.println(DELIMITER);
        System.out.println("The oldest candidate: " + candidateStatistics.getOldestCandidateName());
        System.out.println(DELIMITER);

        System.out.println();

        System.out.println(DELIMITER);
        System.out.println("ELECTION STATISTICS");
        System.out.println(DELIMITER);

        ElectionStatistics electionStatistics = new ElectionStatisticsImpl(dataLoader.getRegionResultsList(datasetEnum), dataLoader.getAbroadResults(datasetEnum), dataLoader.getCandidates(datasetEnum));

        System.out.println("Total number of voters: " + electionStatistics.getTotalVoterCount());
        System.out.println("Total number of issued envelopes: " + electionStatistics.getTotalIssuedEnvelopes());
        System.out.println("Total number of valid votes: " + electionStatistics.getTotalValidVotes());
        System.out.println("Total number of invalid votes: " + electionStatistics.getTotalInvalidVotes());
        System.out.println("Total voter turnout: " + electionStatistics.getTotalVoterTurnout());
        System.out.println("Candidate votes: " + electionStatistics.getCandidateVotesMap());
        System.out.println("Candidate percent: " + electionStatistics.getCandidateVotesPercentMap());
        System.out.println("Candidate list sorted: " + electionStatistics.getCandidatesByVotesDesc());

        System.out.println();

        System.out.println(DELIMITER);
        System.out.println("ABROAD ELECTION STATISTICS");
        System.out.println(DELIMITER);

        AbroadStatistics abroadStatistics = new AbroadStatisticsImpl(dataLoader.getAbroadResults(datasetEnum), dataLoader.getCandidates(datasetEnum));

        System.out.println("Country with the highest non valid votes ratio: " + abroadStatistics.getNameOfCountryWithTheHighestNonValidVotesRatio());
        System.out.println("Candidate victory map: " + abroadStatistics.getCandidateVictoryCountryMap());
        System.out.println("Continent registered votes count: " + abroadStatistics.getContinentRegisteredVoterCountMap());
        System.out.println("County with the most registered voters: " + abroadStatistics.getNameOfCountryWithMostRegisteredVoters());

        System.out.println();

        System.out.println(DELIMITER);
        System.out.println("NATIONAL ELECTION STATISTICS");
        System.out.println(DELIMITER);

        NationalStatistics nationalStatistics = new NationalStatisticsImpl(dataLoader.getRegionResultsList(datasetEnum), dataLoader.getCandidates(datasetEnum));

        System.out.println("City with the lowest voter turnout: " + nationalStatistics.getNameOfCityWithTheLowestVoterTurnout());
        System.out.println("City with the highest non valid votes ratio: " + nationalStatistics.getNameOfCityWithTheHighestNonValidVotesRatio());
        System.out.println("(In case of 2023 dataset) Top 10 City where Petr Pavel won: " + nationalStatistics.getTop10CitiesWhereCandidateWonOrderedByNumberOfVotesDesc(4));
        System.out.println("(In case of 2023 dataset) Top 10 City where Andrej Babiš won: " + nationalStatistics.getTop10CitiesWhereCandidateWonOrderedByNumberOfVotesDesc(7));
        System.out.println("Region winner map: " + nationalStatistics.getRegionWinnerMap());
    }
}
