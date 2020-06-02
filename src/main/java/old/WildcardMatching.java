package old;

public class WildcardMatching {
    static boolean match(String pattern, String string) {

        // If we reach at the end of both strings,
        // we are done
        if (pattern.length() == 0 && string.length() == 0)
            return true;

        // Make sure that the characters after '*'
        // are present in the string.
        // This function assumes that the pattern
        // will not contain two consecutive '*'
        if (pattern.length() > 1 && pattern.charAt(0) == '*' && string.length() == 0)
            return false;

        // If the first string contains '?',
        // or current characters of both strings match
        if ((pattern.length() > 1 && pattern.charAt(0) == '?') ||
                (pattern.length() != 0 && string.length() != 0 && pattern.charAt(0) == string.charAt(0))
            )
            return match(pattern.substring(1), string.substring(1));

        // If there is *, then there are two possibilities
        // a) We consider current character of second string
        // b) We ignore current character of second string.
        if (pattern.length() > 0 && pattern.charAt(0) == '*')
            return match(pattern.substring(1), string) || match(pattern, string.substring(1));

        return false;
    }

    // A function to run test cases
    static void test(String first, String second)
    {
        if (match(first, second))
            System.out.println("Yes");
        else
            System.out.println("No");
    }

    // Driver Code
    public static void main(String[] args)
    {
        test("g*ks", "geeks"); // Yes
        test("ge?ks*", "geeksforgeeks"); // Yes
        test("g*k", "gee"); // No because 'k' is not in second
        test("*pqrs", "pqrst"); // No because 't' is not in first
        test("abc*bcd", "abcdhghgbcd"); // Yes
        test("abc*c?d", "abcd"); // No because second must have 2
        // instances of 'c'
        test("*c*d", "abcd"); // Yes
        test("*?c*d", "abcd"); // Yes
    }

}
