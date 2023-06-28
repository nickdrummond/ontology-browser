package org.coode.www.util;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class PairwiseOrdering<T> {

    LinkedList<LinkedList<T>> lists = new LinkedList<>();

    public void add(List<T> newlist) {
        merge(newlist);
    }

    private void merge(List<T> newList) {
        T first = newList.get(0);
        T last = newList.get(newList.size()-1);

        for (List<T> existing : new LinkedList<>(lists)) {
            if (existing.get(existing.size()-1).equals(first)) {
                lists.remove(existing);
                // concat at end
                existing.addAll(newList.subList(1, newList.size()));
                add(existing);
                return;
            }
            else if (existing.get(0).equals(last)) {
                lists.remove(existing);
                //concat at start
                existing.addAll(0, newList.subList(0, newList.size()-1));
                add(existing);
                return;
            }
        }
        lists.add(new LinkedList<>(newList));
    }

    public LinkedList<LinkedList<T>> getResult() {
        return lists;
    }

    public List<T> flattened() { return lists.stream().flatMap(List::stream).collect(Collectors.toList());}
}
