function drawfitline(A)
% function drawfitline(A)
%
% INPUT: 
% A: 2-by-n matrix
%    with data points in 2D
%
[U, C, b0] = fitaff(A, 1);
u = U(:,1);
t = linspace(min(C), max(C), 2);
line = b0 + u * t;
hold on;
plot(line(1, :), line(2, :), 'g-');
plot(A(1, :), A(2, :), 'rx');
for i = 1:size(A, 2)
    proj = b0 + U * C(:, i);
    plot([A(1, i), proj(1)], [A(2, i), proj(2)], 'r-');
end
hold off;
axis equal;
return
