function [x, y, r, success] = LM_iter(X, x0, y0, r0, mu)
% [x y r success] = LM_iter(X, x0, y0, r0, mu)
%
% makes the Levenberg-Marquardt iteration. 
%
% INPUT: 
% X: n-by-2 matrix
%    with data
% x0, y0 are the coordinates of the circle center.
% r0 is the circle radius
% mu is the damping factor (the factor which multiplies the
% regularizing identity matrix)

% OUTPUT: temp
% success == 1 if the iteration is successful, i.e. 
% value of criterion f is decreased after the update 
% of x. 
% success == 0 in the oposite case. 
%
% x,y,r are updated parameters if success == 1. 
% x,y,r = x0,y0,r0 if success == 0.
    n = size(X,1);
    d = dist(X, x0, y0, r0);
    J = zeros(n, 3);
    t = sqrt((X(:,1)-x0).^2 + (X(:,2)-y0).^2);
    J(:,1) = (x0 - X(:,1)) ./ t;
    J(:,2) = (y0 - X(:,2)) ./ t;
    J(:,3) = -1;
    A = J' * J + mu * eye(3);
    delta = -A \ (J' * d);
    sols = [x0; y0; r0] + delta;
    f = sum(d.^2);
    d = dist(X, sols(1), sols(2), sols(3));
    f_new = sum(d.^2);
    success = f_new < f;
    if success
        x = sols(1);
        y = sols(2);
        r = sols(3);
    else
        x = x0;
        y = y0;
        r = r0;
    end
end
