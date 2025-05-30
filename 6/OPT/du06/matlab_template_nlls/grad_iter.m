function [x, y, r] = grad_iter(X, x0, y0, r0, a)
% [x y r] = grad_iter(X, x0, y0, r0, a)
%
% makes the gradient method iteration. 
%
% INPUT: 
% X: n-by-2 matrix
%    with data
% x0, y0 are the coordinates of the circle center.
% r0 is the circle radius
% a is the stepsize
%
% OUTPUT: 
% coordinates and radius after one step of gradient method.
    d = dist(X, x0, y0, r0);
    dx = sum(2*d .* (x0 - X(:,1)) ./ sqrt((X(:,1)-x0).^2 + (X(:,2)-y0).^2));
    dy = sum(2*d .* (y0 - X(:,2)) ./ sqrt((X(:,1)-x0).^2 + (X(:,2)-y0).^2));
    dr = -2 * sum(d);
    g = [dx; dy; dr];
    sols = [x0; y0; r0] - a * g;
    x = sols(1);
    y = sols(2);
    r = sols(3);
end
