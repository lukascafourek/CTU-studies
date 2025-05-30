function x = vyhra(c, k)
    f = [0 0 0 0 0 -1];
    A = [-c(1), -c(2), 0, 0, 0, 1;
        0, -c(2), -c(3), -c(4), 0, 1;
        0, 0,  0, -c(4), -c(5), 1];
    b = [0 0 0];
    Aeq = [1 1 1 1 1 0];
    beq = k;
    lb = [0 0 0 0 0 0];
    ub = [];
    sol = linprog(f, A, b, Aeq, beq, lb, ub);
    x = sol(1:5);
end
