
package cz.cvut.fel.system.provider;

public interface HashProvider {
    /**
     * Calculates hash of the given string
     * @param s string
     * @return hash(s)
     */
    public String computeHash(String s);
}
