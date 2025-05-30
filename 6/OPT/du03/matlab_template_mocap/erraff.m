function d = erraff(A)
% function d = erraff(A)
%
% INPUT: 
% A: m-by-n matrix
%    with data
%
% OUTPUT:
% d: m-by-1 matrix
%
A_centered = A - mean(A, 2);

[~, S, ~] = svd(A_centered, 'econ');
sigma = diag(S).^2;
d = flipud(cumsum(flipud(sigma)));

% [~, eig_values] = eig(A_centered * A_centered');
% lambda = sort(diag(eig_values), 'descend');
% d = flipud(cumsum(flipud(lambda)));
return
