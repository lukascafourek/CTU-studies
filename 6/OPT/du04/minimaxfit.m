function [a, b, r] = minimaxfit(x, y)
[n, m] = size(x);
numVars = n + 2;
f = zeros(numVars, 1);
f(end) = 1;
A = zeros(2 * m, numVars);
b = zeros(2 * m, 1);
for i = 1:m
    xi = x(:, i)';
    yi = y(i);
    A(i, 1:n) = xi;
    A(i, n + 1) = 1;
    A(i, n + 2) = -1;
    b(i) = yi;
    A(i + m, 1:n) = -xi;
    A(i + m, n + 1) = -1;
    A(i + m, n + 2) = -1;
    b(i + m) = -yi;
end
Aeq = [];
beq = [];
lb = [-inf(n + 1, 1); 0];
ub = [];
sol = linprog(f, A, b, Aeq, beq, lb, ub);
a = sol(1:n);
b = sol(n + 1);
r = sol(end);
end
