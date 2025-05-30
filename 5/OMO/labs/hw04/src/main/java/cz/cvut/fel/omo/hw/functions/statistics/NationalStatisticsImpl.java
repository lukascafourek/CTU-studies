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

public class NationalStatisticsImpl implements NationalStatistics {

    private final CompletableFuture<List<RegionResults>> regionResults;
    private final CandidateUtils candidateUtils;

    public NationalStatisticsImpl(CompletableFuture<List<RegionResults>> regionResults, CompletableFuture<Candidates> candidates) {
        this.regionResults = regionResults;
        this.candidateUtils = new CandidateUtilsImpl(candidates);
    }

    @Override
    public String getNameOfCityWithTheLowestVoterTurnout() {
        return CompletableFutureUtils.applyAndGet(regionResults, regions -> regions.stream()
                .flatMap(regionResult -> regionResult.getRegions().stream())
                .flatMap(region -> region.getDistricts().stream())
                .flatMap(district -> district.getCities().stream())
                .min(Comparator.comparingDouble(city ->
                        (double) city.getVoterTurnout().getNumberOfSubmittedVotingEnvelopes() /
                        city.getVoterTurnout().getNumberOfRegisteredVoters()))
                .map(City::getName).orElse("Unknown"));
    }

    @Override
    public String getNameOfCityWithTheHighestNonValidVotesRatio() {
        return CompletableFutureUtils.applyAndGet(regionResults, regions -> regions.stream()
                .flatMap(regionResult -> regionResult.getRegions().stream())
                .flatMap(region -> region.getDistricts().stream())
                .flatMap(district -> district.getCities().stream())
                .max(Comparator.comparingDouble(city ->
                        (double) (city.getVoterTurnout().getNumberOfSubmittedVotingEnvelopes() -
                                city.getVoterTurnout().getNumberOfValidVotes()) /
                                city.getVoterTurnout().getNumberOfSubmittedVotingEnvelopes()))
                .map(City::getName).orElse("Unknown"));
    }

    @Override
    public List<String> getTop10CitiesWhereCandidateWonOrderedByNumberOfVotesDesc(int candidateId) {
        return CompletableFutureUtils.applyAndGet(regionResults, regions -> regions.stream()
                .flatMap(regionResult -> regionResult.getRegions().stream())
                .flatMap(region -> region.getDistricts().stream())
                .flatMap(district -> district.getCities().stream())
                .filter(city -> city.getVotes().stream().anyMatch(vote ->
                        vote.getCandidateId() == candidateId && vote.getVotes() == city.getVotes().stream()
                                .mapToInt(Vote::getVotes).max().orElse(0)))
                .sorted(Comparator.comparingInt((City city) -> city.getVotes().stream()
                        .filter(vote -> vote.getCandidateId() == candidateId)
                        .mapToInt(Vote::getVotes).sum()).reversed())
                .map(City::getName).limit(10).collect(Collectors.toList()));
    }

    @Override
    public Map<String, String> getRegionWinnerMap() {
        return CompletableFutureUtils.applyAndGet(regionResults, regions -> regions.stream()
                .flatMap(regionResult -> regionResult.getRegions().stream())
                .collect(Collectors.toMap(
                        Region::getName,
                        region -> region.getDistricts().stream()
                                .flatMap(district -> district.getCities().stream())
                                .flatMap(city -> city.getVotes().stream())
                                .collect(Collectors.groupingBy(
                                        Vote::getCandidateId,
                                        Collectors.summingInt(Vote::getVotes)))
                                .entrySet().stream()
                                .max(Map.Entry.comparingByValue())
                                .map(entry -> candidateUtils.getCandidateFullName(entry.getKey())
                                        .orElse("Unknown")).orElse("Unknown"))));
    }
}
