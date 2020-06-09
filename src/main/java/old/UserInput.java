package old;

public class UserInput {

    public static void main(String[] args) {
        TextInput input = new NumericInput();
        input.add('1');
        input.add('a');
        input.add('0');
        System.out.println(input.getValue());
    }
}

class TextInput {
    String current = "";

    void add(Character c) {
        this.current = current + c.toString();
    }

    String getValue() {
        return this.current;
    }

}
class NumericInput extends TextInput {
    @Override
    void add(Character c) {
        if (c >= '0' && c <= '9')
            super.add(c);
    }
}