package com.sondv.control;

import com.sondv.model.KeyValue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.Predicate;

/**
 * An utility class which provides functionality to work with built-in java collection.
 */
public final class CollectionUtil {

    /**
     * Zip two lists into a list of {@link KeyValue}.
     * The type of the first list is a key.
     * The type of the second list is a value.
     * For example:
     * <pre>
     * {@code
     * List<String> l1 = Arrays.asList("a", "b");
     * List<Integer> l2 = Arrays.asList(1, 2);
     * List<KeyValue<String, Integer>> result = zip2(l1, l2);
     * }
     * </pre>
     * which is an array of pairs: {@code [("a", 1), ("b", 2)]}
     *
     */
    public static <T1, T2> List<KeyValue<T1, T2>> zip2(List<T1> list1, List<T2> list2) {
        final int l1 = list1.size();
        final int l2 = list2.size();
        final int maxLength = l1 > l2 ? l2 : l1;
        List<KeyValue<T1, T2>> out = new ArrayList<>();

        for (int i = 0; i < maxLength; i++) {
            out.add(new KeyValue<>(list1.get(i), list2.get(i)));
        }
        return out;
    }

    /**
     * Find the first element in a collection that matched a predicate. */
    public static <R> Try<R> findFirst(Collection<R> source, Predicate<? super R> condition) {
        for (R element : source) {
            if (condition.test(element)) {
                return Try.of(() -> element);
            }
        }
        return Try.failure(new NoSuchElementException("Got no match"));
    }

    /**
     * Find all elements in a collection that matched a predicate. */
    public static <R> List<R> findAll(Collection<R> source, Predicate<? super R> condition) {
        ArrayList<R> out = new ArrayList<>();
        for (R element : source) {
            if (condition.test(element)) {
                out.add(element);
            }
        }
        return out;
    }
}
