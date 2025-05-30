package cz.cvut.fel.omo.cv8.expressions;

import com.google.common.collect.ImmutableList;
import cz.cvut.fel.omo.cv8.visitors.ListExpressionVisitor;

public class IntList implements ListExpression {

    private final ImmutableList<Integer> list;

    private IntList(ImmutableList<Integer> list) {
        this.list = list;
    }

    public static IntList empty() {
        return new IntList(ImmutableList.of());
    }

    public static IntList of(int value) {
        return new IntList(ImmutableList.of(value));
    }

    public static IntList of(ImmutableList<Integer> list) {
        return new IntList(list);
    }

    public int size() {
        return list.size();
    }

    public ImmutableList<Integer> getList() {
        return list;
    }

    @Override
    public ImmutableList<Integer> evaluate(Context c) {
        return list;
    }

    @Override
    public void accept(ListExpressionVisitor v) {
        v.visitIntList(this);
    }
}
