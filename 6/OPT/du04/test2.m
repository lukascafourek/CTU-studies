x = [1 2 3 3 2 ; 4 1 2 5 6; 7 8 9 -5 7];
y = [7 4 1 2 5];
[a, b, r] = minimaxfit(x,y);
 
% a = [-2.776 0.194 -0.030]
% b = 9.403
% r = 0.194

x2 = [4 1 2 5 6];
[a2, b2, r2] = minimaxfit(x2, y);
plotline(x2, y, a2, b2, r2);
