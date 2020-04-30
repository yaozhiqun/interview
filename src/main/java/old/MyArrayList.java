package old;

import java.util.Arrays;

public class MyArrayList<T> {

    private Object[] store;
    private int capacity = 8;
    private int size = 0;

    public MyArrayList() {
        store = new Object[capacity];
    }

    public Object get(int index) {
        checkIndex(index);
        return store[index];
    }

    public void add(Object object) {
        if (store.length - size < 4)
            increaseSize();

        store[size++] = object;
    }

    public void remove(int index) {
        checkIndex(index);
        store[index] = null;
        int i = index;
        while (i < size) {
            store[i] = store[i + 1];
            store[i + 1] = null;
            i ++;
        }

        size--;
    }

    public int size() {
        return size;
    }

    private void increaseSize() {
        int newSize = store.length * 2;
        store = Arrays.copyOf(store, newSize);
        System.out.println("Auto increased list size to " + newSize);
    }

    private void checkIndex(int index) {
        if (index > size)
            throw new IndexOutOfBoundsException();
    }

    public static void main(String[] args) {
        MyArrayList list = new MyArrayList();
        list.add("A");
        list.add("B");
        list.add("C");
        list.add("D");
        list.add("E");
        list.add("F");
        list.add("G");
        list.add("H");
        list.add("I");
        list.add("J");
        list.add("K");
        System.out.println(list.size());
        list.remove(0);
        System.out.println(list.size());
        for (int i = 0; i < list.size(); i ++)
            System.out.println(list.get(i));
    }
}
