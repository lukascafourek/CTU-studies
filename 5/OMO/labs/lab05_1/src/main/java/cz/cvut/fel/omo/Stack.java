package cz.cvut.fel.omo;

public interface Stack {

    /*
     *  Method for inserting new element on top of the stack.
     *  @param int Value to insert
     */
    void push(int toInsert);

    /*
     *  Method for removing element from top of the stack
     *  @return int Element from top of the stack.
     */
    int pop();

    /*
     *  Method for determining, whether stack is empty.
     *  @return boolean Empty stack indicator.
     */
    boolean isEmpty();

    /*
     *  Method for retrieving depth of stack.
     *  @return int Current amount of elements in stack.
     */
    int getSize();
}
