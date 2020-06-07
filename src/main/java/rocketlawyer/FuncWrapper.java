package rocketlawyer;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

class Tester {
    static int counter = -1;

    public Object test() {
        counter ++;
        if (counter == 0)
            return 100;
        else if (counter == 1)
            throw new RuntimeException("Oops!");
        else
            return 99;
    }
}
public class FuncWrapper {
    static List<Supplier<Object>> throwErrors = new ArrayList<>();

    Object invoke(Supplier<Object> func) {
        if (throwErrors.contains(func)) {
//            System.out.println("Avoid func as it threw error");
            return null;
        }

        try {
//            System.out.println("Invoke func");
            return func.get();
        } catch (Throwable t) {
            throwErrors.add(func);
            return null;
        }
    }

    public static void main(String[] args) {
        FuncWrapper wrapper = new FuncWrapper();
        Tester tester = new Tester();
        Supplier<Object> func = tester::test;
        System.out.println(wrapper.invoke(func));
        System.out.println(wrapper.invoke(func));
        System.out.println(wrapper.invoke(func));
    }
}
