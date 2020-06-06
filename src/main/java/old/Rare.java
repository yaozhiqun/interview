package old;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

/**
 * @author jamburn
 *
 */
public class Rare {

    static boolean DEBUG = false;

    static void debug(String s) {
        if (DEBUG)
            System.out.println(s);
    }

    public static int nthMostRare(int[] elements, int n) {

        Map<Integer, Integer> freqHash = new HashMap<Integer, Integer>();
        for (int i = 0; i < elements.length; i++)
        {
            int element = elements[i];
            if (freqHash.containsKey(element))
            {
                freqHash.put(element, freqHash.get(element) + 1);
            }
            else
            {
                freqHash.put(element, 1);
            }
        }

        Iterator it = freqHash.entrySet().iterator();

        ArrayList<Entry<Integer, Integer>> entryList = new ArrayList<Entry<Integer, Integer>>();

        EntryComperator ec = new EntryComperator();

        while (it.hasNext())
        {
            Entry<Integer, Integer> pair = (Entry<Integer, Integer>) it.next();
            entryList.add(pair);
        }

        entryList.sort(ec);

        int nthMostRare = 0;

        for (int i = 0; i < entryList.size(); i++)
        {

            Entry<Integer, Integer> pair = entryList.get(i);
            if (i == n - 1)
            {
                nthMostRare = pair.getKey();
                break;
            }

        }
        return nthMostRare;
    }

    public static void main(String[] args) {
        int x = nthMostRare(new int[] { 5, 4, 3, 2, 1, 5, 4, 3, 2, 5, 4, 3, 5, 4, 5 }, 4);
        System.out.println(x);
    }

}

class EntryComperator implements Comparator<Entry<Integer,Integer>>
{

    @Override
    public int compare(Entry<Integer, Integer> o1, Entry<Integer, Integer> o2) {

        if (o1.getValue() > o2.getValue())
            return 1;
        else if (o1.getValue() < o2.getValue())
            return -1;
        else
            return 0;

    }

}
