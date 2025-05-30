package cz.cvut.fel.omo.hw.functions.utils;

import cz.cvut.fel.omo.hw.functions.data.model.*;
import lombok.RequiredArgsConstructor;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;

@RequiredArgsConstructor
public class ElectionsUtilsImpl implements ElectionsUtils {

    private final CompletableFuture<List<RegionResults>> regionResultsFuture;
    private final CompletableFuture<AbroadResults> abroadResultsFuture;

    @Override
    public List<Vote> getAllVotes() {
        return Stream.concat(CompletableFutureUtils.applyAndGet(regionResultsFuture,
                        regionResults -> regionResults.stream()
                        .flatMap(regions -> regions.getRegions().stream())
                        .flatMap(region -> region.getDistricts().stream())
                        .flatMap(district -> district.getCities().stream())
                        .flatMap(city -> city.getVotes().stream())),
                Optional.ofNullable(CompletableFutureUtils.applyAndGet(abroadResultsFuture,
                        abroadResults -> abroadResults.getAbroad()
                        .getContinents().stream()
                        .flatMap(continent -> continent.getCountries().stream())
                        .flatMap(country -> country.getVotes().stream()))).orElse(Stream.empty())).toList();
    }

    @Override
    public List<VoterTurnout> getAllVoterTurnouts() {
        return Stream.concat(CompletableFutureUtils.applyAndGet(regionResultsFuture,
                        regionResults -> regionResults.stream()
                        .flatMap(regions -> regions.getRegions().stream())
                        .flatMap(region -> region.getDistricts().stream())
                        .flatMap(district -> district.getCities().stream())
                        .map(City::getVoterTurnout)),
                Optional.ofNullable(CompletableFutureUtils.applyAndGet(abroadResultsFuture,
                abroadResults -> abroadResults.getAbroad()
                        .getContinents().stream()
                        .flatMap(continent -> continent.getCountries().stream())
                        .map(Country::getVoterTurnout))).orElse(Stream.empty())).toList();
    }
}
