package cz.cvut.fel.omo.cv8.expressions;

import com.google.common.collect.ImmutableList;
import cz.cvut.fel.omo.cv8.visitors.ListExpressionVisitor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Remove implements ListExpression {

    private final ListExpression sub;
    private int element;

    public Remove(ListExpression sub, int element) {
        this.sub = sub;
        this.element = element;
    }

    public ListExpression getSub() {
        return sub;
    }

    public int getElement() {
        return element;
    }

    @Override
    public ImmutableList<Integer> evaluate(Context c) {
        List<Integer> mutableList = new ArrayList<>(sub.evaluate(c));
        mutableList.removeAll(Collections.singleton(element));
        return ImmutableList.copyOf(mutableList);
    }

    @Override
    public void accept(ListExpressionVisitor v) {
        v.visitRemove(this);
    }
}
