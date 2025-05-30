package cz.cvut.fel.omo.hw.functions.utils;


import cz.cvut.fel.omo.hw.functions.data.DataLoader;
import cz.cvut.fel.omo.hw.functions.data.DatasetEnum;
import cz.cvut.fel.omo.hw.functions.data.OfflineDataLoader;
import cz.cvut.fel.omo.hw.functions.data.model.Candidate;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Optional;

class CandidateUtilsImplTest {

    private static CandidateUtils candidateUtils;

    @BeforeAll
    static void setup() {
        DataLoader dataLoader = new OfflineDataLoader();
        candidateUtils = new CandidateUtilsImpl(dataLoader.getCandidates(DatasetEnum.DATASET_2023_2ND_ROUND));
    }

    @Test
    @DisplayName("Test candidate full name.")
    void getCandidateFullName() {
        Assertions.assertEquals(Optional.of("Danuše Nerudová"), candidateUtils.getCandidateFullName(6));
        Assertions.assertEquals(Optional.of("Marek Hilšer"), candidateUtils.getCandidateFullName(9));
        Assertions.assertEquals(Optional.empty(), candidateUtils.getCandidateFullName(90));
    }

    @Test
    @DisplayName("Test candidate age.")
    void getCandidateAge() {
        Assertions.assertEquals(Optional.of(55), candidateUtils.getCandidateAge(3));
        Assertions.assertEquals(Optional.of(68), candidateUtils.getCandidateAge(7));
        Assertions.assertEquals(Optional.empty(), candidateUtils.getCandidateAge(-1));
    }

    @Test
    @DisplayName("Test candidate attribute by function.")
    void getCandidateAttribute() {
        Assertions.assertEquals("Andrej", candidateUtils.getCandidateAttribute(7, Candidate::getForename).orElseThrow());
        Assertions.assertEquals(61, candidateUtils.getCandidateAttribute(4, Candidate::getAge).orElseThrow());
        Assertions.assertEquals(Optional.empty(), candidateUtils.getCandidateAttribute(-1, Candidate::getSurname));
    }

    @Test
    @DisplayName("Test get candidate by id.")
    void getCandidate() {
        Assertions.assertEquals(1, candidateUtils.getCandidate(1).map(Candidate::getId).orElseThrow());
        Assertions.assertEquals("Pavel Fischer", candidateUtils.getCandidate(1).map(Candidate::getFullName).orElseThrow());
        Assertions.assertEquals(Optional.empty(), candidateUtils.getCandidate(-1));
    }
}
