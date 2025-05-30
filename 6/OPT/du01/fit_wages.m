function x = fit_wages(t, M)
    A = [ones(length(t), 1), t];
    x = A \ M;
end
