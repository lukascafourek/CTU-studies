package cz.cvut.fel.omo.hw.functions.statistics;

import cz.cvut.fel.omo.hw.functions.data.model.*;
import cz.cvut.fel.omo.hw.functions.utils.CandidateUtils;
import cz.cvut.fel.omo.hw.functions.utils.CandidateUtilsImpl;
import cz.cvut.fel.omo.hw.functions.utils.ElectionsUtils;
import cz.cvut.fel.omo.hw.functions.utils.ElectionsUtilsImpl;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class ElectionStatisticsImpl implements ElectionStatistics {

    private final ElectionsUtils electionsUtils;
    private final CandidateUtils candidateUtils;

    public ElectionStatisticsImpl(CompletableFuture<List<RegionResults>> regionResultsFuture, CompletableFuture<AbroadResults> abroadResultsFuture, CompletableFuture<Candidates> candidatesFuture) {
        this.candidateUtils = new CandidateUtilsImpl(candidatesFuture);
        this.electionsUtils = new ElectionsUtilsImpl(regionResultsFuture, abroadResultsFuture);
    }

    @Override
    public int getTotalValidVotes() {
        return electionsUtils.getAllVoterTurnouts().stream()
                .mapToInt(VoterTurnout::getNumberOfValidVotes).sum();
    }

    @Override
    public int getTotalInvalidVotes() {
        return electionsUtils.getAllVoterTurnouts().stream()
                .mapToInt(VoterTurnout::getNumberOfSubmittedVotingEnvelopes).sum() - getTotalValidVotes();
    }

    @Override
    public int getTotalVoterCount() {
        return electionsUtils.getAllVoterTurnouts().stream()
                .mapToInt(VoterTurnout::getNumberOfRegisteredVoters).sum();
    }

    @Override
    public int getTotalIssuedEnvelopes() {
        return electionsUtils.getAllVoterTurnouts().stream()
                .mapToInt(VoterTurnout::getNumberOfIssuedVotingEnvelopes).sum();
    }

    @Override
    public double getTotalVoterTurnout() {
        return (double) getTotalIssuedEnvelopes() / getTotalVoterCount() * 100;
    }

    @Override
    public Map<String, Integer> getCandidateVotesMap() {
        return electionsUtils.getAllVotes().stream()
                .collect(Collectors.groupingBy(
                        vote -> candidateUtils.getCandidateFullName(vote.getCandidateId()).orElse("Unknown"),
                        Collectors.summingInt(Vote::getVotes)));
    }

    @Override
    public Map<String, Double> getCandidateVotesPercentMap() {
        return getCandidateVotesMap().entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        entry -> (double) entry.getValue() / getTotalValidVotes() * 100));
    }

    @Override
    public String getCandidatesByVotesDesc() {
        return getCandidateVotesMap().entrySet().stream()
                .sorted((entry1, entry2) -> Integer.compare(
                entry2.getValue(), entry1.getValue()))
                .map(entry -> String.format("%s (%.2f%%)", entry.getKey(),
                        getCandidateVotesPercentMap().get(entry.getKey())))
                .collect(Collectors.joining(", "));
    }
}
