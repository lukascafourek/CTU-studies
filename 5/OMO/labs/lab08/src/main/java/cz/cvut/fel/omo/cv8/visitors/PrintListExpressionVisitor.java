package cz.cvut.fel.omo.cv8.visitors;

import cz.cvut.fel.omo.cv8.expressions.*;

public class PrintListExpressionVisitor implements ListExpressionVisitor {

    @Override
    public void visitIntList(IntList v) {
        System.out.print(v.getList());
    }

    @Override
    public void visitVarList(VarList v) {
        System.out.print(v.getName());
    }

    @Override
    public void visitRemove(Remove remove) {
        System.out.print("R (");
        remove.getSub().accept(this);
        System.out.print(", " + remove.getElement() + ")");
    }

    @Override
    public void visitConcatenate(Concatenate concatenate) {
        System.out.print("C (");
        concatenate.getLeft().accept(this);
        System.out.print(", ");
        concatenate.getRight().accept(this);
        System.out.print(")");
    }

    @Override
    public void visitUnique(Unique unique) {
        System.out.print("U (");
        unique.getSub().accept(this);
        System.out.print(")");
    }
}
