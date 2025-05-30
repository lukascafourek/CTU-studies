package cz.cvut.fel.omo.cv8.expressions;

import com.google.common.collect.ImmutableList;
import cz.cvut.fel.omo.cv8.visitors.ListExpressionVisitor;

import java.util.*;

public class Unique implements ListExpression {

    private final ListExpression sub;

    public Unique(ListExpression sub) {
        this.sub = sub;
    }

    public ListExpression getSub() {
        return sub;
    }

    @Override
    public ImmutableList<Integer> evaluate(Context c) {
        List<Integer> originalList = new ArrayList<>(sub.evaluate(c));
        List<Integer> uniqueList = new ArrayList<>(new LinkedHashSet<>(originalList));
        return ImmutableList.copyOf(uniqueList);
    }

    @Override
    public void accept(ListExpressionVisitor v) {
        v.visitUnique(this);
    }
}
