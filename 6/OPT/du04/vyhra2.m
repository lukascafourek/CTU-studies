function x = vyhra2(c, k, m)
    f = [0 0 0 -1];
    A = [-c(1), 0, 0, 1;
        0, -c(2), 0, 1;
        0, 0, -c(3), 1];
    b = [0 0 0];
    Aeq = [1 1 1 0];
    beq = k;
    lb = [m m m 0];
    ub = [];
    sol = linprog(f, A, b, Aeq, beq, lb, ub);
    x = sol(1:3);
end
