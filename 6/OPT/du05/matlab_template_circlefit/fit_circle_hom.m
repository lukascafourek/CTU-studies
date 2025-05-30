function [d, e, f] = fit_circle_hom(X)
% function [d e f] = fit_circle_hom(X)
%
% INPUT: 
% X: n-by-2 vector
%    with data
%
% OUTPUT: 
% quadric coordinates of the circle
    xi = X(:, 1);
    yi = X(:, 2);
    D = [xi.^2 + yi.^2, xi, yi, ones(size(xi))];
    [~, ~, V] = svd(D, 0);
    sols = V(:, end);
    a = sols(1);
    d = sols(2) / a;
    e = sols(3) / a;
    f = sols(4) / a;
end
