package genericmethodtest;

public class Container<T> {

    private T t;

    public void add(T t) {
        this.t = t;
    }

    public T get() {
        return t;
    }

    public static <E> void printArray(E[] inputArray) {
        for (E element : inputArray) {
            System.out.printf("%s ", element);
        }
        System.out.println();
    }

    public static void main(String[] args) {
        /* part 1 */
        Container<Integer> integerBox = new Container<Integer>();
        Container<String> stringBox = new Container<String>();

        integerBox.add(10);
        stringBox.add("Hello World");

        System.out.printf("Integer Value: %d\n\n", integerBox.get());
        System.out.printf("String Value: %s\n", stringBox.get());

        /* part 2 */
        // Create arrays of Integer, Double and Character
        Integer[] intArray = {1, 2, 3, 4, 5};
        Double[] doubleArray = {1.1, 2.2, 3.3, 4.4};
        Character[] charArray = {'T', 'E', 'S', 'T'};

        System.out.println("Array integerArray contains:");
        printArray(intArray);   // pass an Integer array

        System.out.println("\nArray doubleArray contains:");
        printArray(doubleArray);   // pass a Double array

        System.out.println("\nArray characterArray contains:");
        printArray(charArray);   // pass a Character array
    }
}