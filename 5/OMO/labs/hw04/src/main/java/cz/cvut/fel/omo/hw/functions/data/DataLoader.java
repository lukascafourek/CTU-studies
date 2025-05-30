package cz.cvut.fel.omo.hw.functions.data;

import cz.cvut.fel.omo.hw.functions.data.model.AbroadResults;
import cz.cvut.fel.omo.hw.functions.data.model.Candidates;
import cz.cvut.fel.omo.hw.functions.data.model.RegionResults;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;

public interface DataLoader {

    /**
     * Loads data about Czech presidency elections abroad.
     *
     * @return Abroad results of Czech presidency elections.
     */
    CompletableFuture<AbroadResults> getAbroadResults(DatasetEnum datasetEnum);

    /**
     * Loads data about Czech presidency elections in Czech regions.
     *
     * @return Regional results of Czech presidency elections.
     */
    CompletableFuture<List<RegionResults>> getRegionResultsList(DatasetEnum datasetEnum);

    /**
     * Loads data about president candidates.
     *
     * @return President candidates data.
     */
    CompletableFuture<Candidates> getCandidates(DatasetEnum datasetEnum);
}
