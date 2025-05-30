package cz.cvut.fel.omo.cv8.visitors;

import com.google.common.collect.ImmutableList;
import cz.cvut.fel.omo.cv8.expressions.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

public class SimplifyListExpressionVisitor implements ListExpressionVisitor {

    private ListExpression value;

    public ListExpression getValue() {
        return value;
    }

    @Override
    public void visitIntList(IntList v) {
        value = v;
    }

    @Override
    public void visitVarList(VarList varList) {
        value = varList;
    }

    @Override
    public void visitRemove(Remove remove) {
        remove.getSub().accept(this);
        ListExpression simplifiedSub = value;
        if (simplifiedSub instanceof IntList) {
            List<Integer> result = new ArrayList<>(((IntList) simplifiedSub).getList());
            result.removeAll(Collections.singleton(remove.getElement()));
            value = IntList.of(ImmutableList.copyOf(result));
        } else {
            value = new Remove(simplifiedSub, remove.getElement());
        }
    }

    @Override
    public void visitConcatenate(Concatenate concatenate) {
        concatenate.getLeft().accept(this);
        ListExpression simplifiedLeft = value;
        concatenate.getRight().accept(this);
        ListExpression simplifiedRight = value;
        if (simplifiedLeft instanceof IntList && simplifiedRight instanceof IntList) {
            List<Integer> result = new ArrayList<>(((IntList) simplifiedLeft).getList());
            result.addAll(((IntList) simplifiedRight).getList());
            value = IntList.of(ImmutableList.copyOf(result));
        } else {
            value = new Concatenate(simplifiedLeft, simplifiedRight);
        }
    }

    @Override
    public void visitUnique(Unique unique) {
        unique.getSub().accept(this);
        ListExpression simplifiedSub = value;
        if (simplifiedSub instanceof IntList) {
            List<Integer> result = new ArrayList<>(new LinkedHashSet<>(((IntList) simplifiedSub).getList()));
            value = IntList.of(ImmutableList.copyOf(result));
        } else {
            value = new Unique(simplifiedSub);
        }
    }
}
