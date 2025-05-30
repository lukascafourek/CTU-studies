package cz.cvut.fel.omo.cv8.visitors;

import cz.cvut.fel.omo.cv8.expressions.*;

public interface ListExpressionVisitor {

    void visitIntList(IntList v);

    void visitVarList(VarList varList);

    void visitRemove(Remove remove);

    void visitConcatenate(Concatenate concatenate);

    void visitUnique(Unique unique);
}
