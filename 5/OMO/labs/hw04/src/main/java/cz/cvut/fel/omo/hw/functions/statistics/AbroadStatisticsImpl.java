package cz.cvut.fel.omo.hw.functions.statistics;

import cz.cvut.fel.omo.hw.functions.data.model.*;
import cz.cvut.fel.omo.hw.functions.utils.CandidateUtils;
import cz.cvut.fel.omo.hw.functions.utils.CandidateUtilsImpl;
import cz.cvut.fel.omo.hw.functions.utils.CompletableFutureUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class AbroadStatisticsImpl implements AbroadStatistics {

    private final CompletableFuture<AbroadResults> abroadResults;
    private final CandidateUtils candidateUtils;

    public AbroadStatisticsImpl(CompletableFuture<AbroadResults> abroadResults, CompletableFuture<Candidates> candidates) {
        this.abroadResults = abroadResults;
        this.candidateUtils = new CandidateUtilsImpl(candidates);
    }

    @Override
    public String getNameOfCountryWithTheHighestNonValidVotesRatio() {
        return CompletableFutureUtils.applyAndGet(abroadResults, results -> results.getAbroad()
                .getContinents().stream()
                .flatMap(continent -> continent.getCountries().stream())
                .max(Comparator.comparingDouble(country ->
                        (double) (country.getVoterTurnout().getNumberOfSubmittedVotingEnvelopes() -
                        country.getVoterTurnout().getNumberOfValidVotes()) /
                                country.getVoterTurnout().getNumberOfSubmittedVotingEnvelopes()))
                .map(Country::getName).orElse("Unknown"));
    }

    @Override
    public Map<String, List<String>> getCandidateVictoryCountryMap() {
        return CompletableFutureUtils.applyAndGet(abroadResults, results -> results.getAbroad().
                getContinents().stream()
                .flatMap(continent -> continent.getCountries().stream())
                .flatMap(country -> country.getVotes().stream()
                        .filter(vote -> vote.getVotes() == country.getVotes().stream()
                                .mapToInt(Vote::getVotes).max().orElse(0)).findFirst()
                        .map(vote -> Map.entry(
                                candidateUtils.getCandidateFullName(vote.getCandidateId()).orElse("Unknown"),
                                country.getName())).stream())
                .collect(Collectors.groupingBy(
                        Map.Entry::getKey,
                        Collectors.mapping(Map.Entry::getValue, Collectors.toList()))));
    }

    @Override
    public Map<String, Integer> getContinentRegisteredVoterCountMap() {
        return CompletableFutureUtils.applyAndGet(abroadResults, results -> results.getAbroad()
                .getContinents().stream()
                .collect(Collectors.toMap(
                        Continent::getName,
                        continent -> continent.getCountries().stream()
                                .mapToInt(country -> country.getVoterTurnout().getNumberOfRegisteredVoters())
                                .sum())));
    }

    @Override
    public String getNameOfCountryWithMostRegisteredVoters() {
        return CompletableFutureUtils.applyAndGet(abroadResults, results -> results.getAbroad()
                .getContinents().stream()
                .flatMap(continent -> continent.getCountries().stream())
                .max(Comparator.comparingInt(country ->
                        country.getVoterTurnout().getNumberOfRegisteredVoters()))
                .map(Country::getName).orElse("Unknown"));
    }
}
