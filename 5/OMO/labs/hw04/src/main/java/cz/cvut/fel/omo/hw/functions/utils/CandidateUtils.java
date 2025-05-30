package cz.cvut.fel.omo.hw.functions.utils;

import cz.cvut.fel.omo.hw.functions.data.model.Candidate;

import java.util.Optional;
import java.util.function.Function;

public interface CandidateUtils {

    /**
     * Returns candidate's full name (for example "Petr Pavel").
     *
     * @param id candidate's id
     * @return Full name of the president candidate in case if valid candidate's id, empty otherwise.
     * @throws IllegalStateException in case of exception when getting the data.
     */
    Optional<String> getCandidateFullName(int id);

    /**
     * Returns candidate's age.
     *
     * @param id candidate's id
     * @return Age of the president candidate in case if valid candidate's id, empty otherwise.
     * @throws IllegalStateException in case of exception when getting the data.
     */
    Optional<Integer> getCandidateAge(int id);

    /**
     * Returns candidate by id.
     *
     * @param id candidate's id
     * @return The president candidate in case if valid candidate's id, empty otherwise.
     * @throws IllegalStateException in case of exception when getting the data.
     */
    Optional<Candidate> getCandidate(int id);

    /**
     * Returns candidate's attribute given by mapping function.
     *
     * @param id              candidate's id
     * @param mappingFunction mapping function to get candidate's attribute
     * @return Attribute of the president candidate in case if valid candidate's id, empty otherwise.
     * @throws IllegalStateException in case of exception when getting the data.
     */
    <T> Optional<T> getCandidateAttribute(int id, Function<Candidate, T> mappingFunction);

}
