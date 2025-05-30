package cz.cvut.fel.omo;

import java.util.EmptyStackException;
import java.util.NoSuchElementException;

/**
 * TODO: implement class using adapter pattern
 */
public class ExtendedStackImpl extends StackImpl implements ExtendedStack {

   private Stack stack = new StackImpl(); // Use adapter pattern here

    @Override
    public void push(int toInsert) {
        stack.push(toInsert);
    }

    @Override
    public void push(int[] toInsert) {
        for (int toInsertElement : toInsert) {
            stack.push(toInsertElement);
        }
    }

    @Override
    public int top() {
        if (stack.isEmpty()) {
            throw new EmptyStackException();
        }
        int top = stack.pop();
        stack.push(top);
        return top;
    }

    @Override
    public int pop() {
        return stack.pop();
    }

    @Override
    public int popFirstNegativeElement() {
        if (stack.isEmpty()) {
            throw new EmptyStackException();
        }
        int count = 0;
        int[] elements = new int[stack.getSize()];
        for (int i = 0; i < stack.getSize(); i++) {
            if (stack.isEmpty()) {
                throw new EmptyStackException();
            }
            int top = stack.pop();
            if (top < 0) {
                for (int j = count - 1; j >= 0; j--) {
                    stack.push(elements[j]);
                }
                return top;
            }
            elements[count] = top;
            count++;
        }
        throw new NoSuchElementException();
    }

    @Override
    public boolean isEmpty() {
        return stack.isEmpty();
    }

    @Override
    public int getSize() {
        return stack.getSize();
    }
}
