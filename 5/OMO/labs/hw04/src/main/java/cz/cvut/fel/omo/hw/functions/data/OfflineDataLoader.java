package cz.cvut.fel.omo.hw.functions.data;

import cz.cvut.fel.omo.hw.functions.data.model.*;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;

import java.io.*;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public class OfflineDataLoader implements DataLoader {

    private static final String FILE_EXT = ".xml";
    private static final String BASE_PATH = "./datasets/";
    private static final String CANDIDATES_FILE = "kandidati";
    private static final String ABROAD_FILE = "zahranici";
    private static final List<String> REGIONS_FILES = List.of("CZ010", "CZ020", "CZ031", "CZ032", "CZ041", "CZ042", "CZ051", "CZ052", "CZ053", "CZ063", "CZ064", "CZ071", "CZ072", "CZ080");
    private static final String CITY_PART_CODE = "MCMO";

    @Override
    public CompletableFuture<AbroadResults> getAbroadResults(DatasetEnum datasetEnum) {
        return CompletableFuture.supplyAsync(() -> loadData(BASE_PATH + datasetEnum.getPath() + "/" + ABROAD_FILE + FILE_EXT, AbroadResults.class));
    }

    @Override
    public CompletableFuture<List<RegionResults>> getRegionResultsList(DatasetEnum datasetEnum) {
        return CompletableFuture.supplyAsync(() -> REGIONS_FILES.stream()
                        .map(fileName -> loadData(BASE_PATH + datasetEnum.getPath() + "/" + fileName + FILE_EXT, RegionResults.class))
                        .toList()
                )
                // remove city parts -> if we count parts and then the whole city, we would count some votes twice
                // this would lead into misleading results in our statistics
                .thenApply(regionResults -> {
                    regionResults.stream()
                            .map(RegionResults::getRegions)
                            .flatMap(Collection::stream)
                            .map(Region::getDistricts)
                            .flatMap(Collection::stream)
                            .forEach(district -> district
                                    .getCities()
                                    .removeIf(city -> CITY_PART_CODE.equals(city.getCityType())));
                    return regionResults;
                });
    }

    @Override
    public CompletableFuture<Candidates> getCandidates(DatasetEnum datasetEnum) {
        return CompletableFuture.supplyAsync(() -> loadData(BASE_PATH + datasetEnum.getPath() + "/" + CANDIDATES_FILE + FILE_EXT, Candidates.class));
    }

    @SuppressWarnings("unchecked")
    private <T> T loadData(final String fileName, Class<T> rootClass) {
        try (InputStream reader = new FileInputStream(fileName)) {
            return (T) JAXBContext.newInstance(rootClass)
                    .createUnmarshaller()
                    .unmarshal(reader);
        } catch (IOException | JAXBException e) {
            throw new IllegalStateException("Cannot parse data from offline dataset!", e);
        }
    }
}
