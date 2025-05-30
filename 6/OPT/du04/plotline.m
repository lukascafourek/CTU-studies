function plotline(x, y, a, b, r)
figure;
hold on;
plot(x, y, 'kx');
xline = linspace(min(x), max(x), 100);
ycenter = a * xline + b;
plot(xline, ycenter, 'r-');
yupper = a * xline + b + r;
ylower = a * xline + b - r;
plot(xline, yupper, 'b-');
plot(xline, ylower, 'b-');
hold off;
axis tight equal;
grid on;
end
