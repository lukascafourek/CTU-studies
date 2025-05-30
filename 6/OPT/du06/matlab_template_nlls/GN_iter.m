function [x, y, r] = GN_iter(X, x0, y0, r0)
% [x y r] = GN_iter(X, x0, y0, r0)
%
% makes the Gauss-Newton iteration. 
%
% INPUT: 
% X: n-by-2 matrix
%    with data
% x0, y0 are the coordinates of the circle center.
% r0 is the circle radius
%
% OUTPUT: 
% coordinates and radius after one step of GN method.
    n = size(X,1);
    d = dist(X, x0, y0, r0);
    J = zeros(n, 3);
    t = sqrt((X(:,1)-x0).^2 + (X(:,2)-y0).^2);
    J(:,1) = (x0 - X(:,1)) ./ t;
    J(:,2) = (y0 - X(:,2)) ./ t;
    J(:,3) = -1;
    delta = -(J' * J) \ (J' * d);
    sols = [x0; y0; r0] + delta;
    x = sols(1);
    y = sols(2);
    r = sols(3);
end
