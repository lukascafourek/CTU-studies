function x = fit_temps(t, T, omega)
    A = [ones(length(t), 1), t, sin(omega * t), cos(omega * t)];
    x = A \ T;
end
