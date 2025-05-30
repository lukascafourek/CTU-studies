function [d, e, f] = fit_circle_nhom(X)
% function [d e f] = fit_circle_nhom(X)
%
% INPUT: 
% X: n-by-2 vector
%    with data
%
%
% OUTPUT: 
% quadric coordinates of the circle
    xi = X(:, 1);
    yi = X(:, 2);
    A = [xi, yi, ones(size(xi))];
    b = -(xi.^2 + yi.^2);
    sols = A \ b;
    d = sols(1);
    e = sols(2);
    f = sols(3);
end
