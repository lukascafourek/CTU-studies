package cz.cvut.fel.omo.cv8.expressions;

import com.google.common.collect.ImmutableList;
import cz.cvut.fel.omo.cv8.visitors.ListExpressionVisitor;

public interface ListExpression {

    ImmutableList<Integer> evaluate(Context c);

    void accept(ListExpressionVisitor v);
}
