package cz.cvut.fel.omo.cv8.expressions;

import com.google.common.collect.ImmutableList;

import java.util.HashMap;
import java.util.Map;

public class Context {

    private final Map<String, ImmutableList<Integer>> variables = new HashMap<>();

    public void put(String name, ImmutableList<Integer> value) {
        variables.put(name, value);
    }

    public ImmutableList<Integer> get(String name) {
        return variables.get(name);
    }
}
