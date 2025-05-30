package cz.cvut.fel.omo.cv8.expressions;

import com.google.common.collect.ImmutableList;
import cz.cvut.fel.omo.cv8.visitors.ListExpressionVisitor;

import java.util.ArrayList;
import java.util.List;

public class Concatenate implements ListExpression {

    private final ListExpression left;
    private final ListExpression right;

    public Concatenate(ListExpression left, ListExpression right) {
        this.left = left;
        this.right = right;
    }

    public ListExpression getLeft() {
        return left;
    }

    public ListExpression getRight() {
        return right;
    }

    @Override
    public ImmutableList<Integer> evaluate(Context c) {
        List<Integer> combined = new ArrayList<>(left.evaluate(c));
        combined.addAll(right.evaluate(c));
        return ImmutableList.copyOf(combined);
    }

    @Override
    public void accept(ListExpressionVisitor v) {
        v.visitConcatenate(this);
    }
}
