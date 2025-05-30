package cz.cvut.fel.omo.hw.functions.data;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum DatasetEnum {

    DATASET_2018_1ST_ROUND("2018", "1"),
    DATASET_2018_2ND_ROUND("2018", "2"),
    DATASET_2023_1ST_ROUND("2023", "1"),
    DATASET_2023_2ND_ROUND("2023", "2");

    private final String directory;
    private final String subDirectory;

    public String getPath() {
        return directory + "/" + subDirectory;
    }

}
