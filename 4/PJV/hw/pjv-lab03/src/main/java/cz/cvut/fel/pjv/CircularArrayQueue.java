package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

/**
 * Implementation of the {@link Queue} backed by fixed size array.
 */
public class CircularArrayQueue implements Queue {

    private final int capacity;
    private int head;
    private int tail;
    private int size;
    private final String [] queue;
    /**
     * Creates the queue with capacity set to the value of 5.
     */
    public CircularArrayQueue() {
        this.capacity = 5;
        this.queue = new String[this.capacity];
        this.size = 0;
        this.head = 0;
        this.tail = 0;
    }


    /**
     * Creates the queue with given {@code capacity}. The capacity represents maximal number of elements that the
     * queue is able to store.
     * @param capacity of the queue
     */
    public CircularArrayQueue(int capacity) {
        this.capacity = capacity;
        this.queue = new String[this.capacity];
        this.size = 0;
        this.head = 0;
        this.tail = 0;
    }

    @Override
    public int size() {
        return this.size;
    }
    @Override
    public boolean isEmpty() {
        return this.size == 0;
    }

    @Override
    public boolean isFull() {
        return this.size == this.capacity;
    }

    @Override
    public boolean enqueue(String obj) {
        if (!isFull() && obj != null) {
            queue[this.tail] = obj;
            this.tail = (this.tail + 1) % this.capacity;
            this.size += 1;
            return true;
        } else {
            return false;
        }
    }

    @Override
    public String dequeue() {
        if (!isEmpty()) {
            String ret = this.queue[this.head];
            this.queue[this.head] = null;
            this.head = (this.head + 1) % this.capacity;
            this.size -= 1;
            return ret;
        } else {
            return null;
        }
    }

    @Override
    public void printAllElements() {
        for (int i = 0; i < this.capacity; ++i) {
            if (this.queue[i] != null) {
                System.out.printf("%s\n", this.queue[i]);
            }
        }
    }
}
