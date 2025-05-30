function d = dist(X, x0, y0, r)
% function d = dist(X,x0,y0,r)
%
% INPUT: 
% X: n-by-2 vector
%    with data
% x0, y0 are the coordinates of the circle center.
% r is the circle radius
%
% OUTPUT: 
% d: 1-by-N vector of *signed* distances of all points 
%    from the circumference. 
    xi = X(:, 1);
    yi = X(:, 2);
    d = sqrt((xi - x0).^2 + (yi - y0).^2) - r;
end
