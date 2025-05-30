function plottraj2(C)
% function plottraj2(C)
%
% INPUT: 
% C: 2-by-m matrix
%    with data
%
hold on;
plot(C(1, :), C(2, :), 'b-');
hold off;
axis equal;
return
