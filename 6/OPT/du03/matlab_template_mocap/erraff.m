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
b0 = mean(A, 2);
A = A - b0;
[~, S, ~] = svd(A, 'econ');
lambda = diag(S).^2;
% [~, eig_values] = eig(A * A');
% lambda = sort(diag(eig_values), 'descend');
total_variance = sum(lambda);
d = total_variance - cumsum(lambda);
return
