package cz.cvut.fel.omo;

public interface Iterator {

    /*
     *  Method for retrieving value of current item.
     *  @return int Value of current item.
     */
    int currentItem();

    /*
     *  Method for shifting iterator to next element of collection.
     *  @return int Value of following element.
     */
    int next();

    /*
     *  Method for verifying, whether the iterator is at the end of data structure.
     *  @return boolean Returns true for last element of collection.
     */
    boolean isDone();

    /*
     *  Method for setting iterator to point at the beginning of the data structure.
     *  @return int First value of collection.
     */
    int first();
}
