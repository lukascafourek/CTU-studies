function [x0, y0, r] = fit_circle_ransac(X, num_iter, threshold)
% function [x0 y0 r] = fit_circle_ransac(X, num_iter, threshold)
%
% INPUT: 
% X: n-by-2 vector
%    with data
% num_iter: number of RANSAC iterations
% threshold: maximal  distance of an inlier from the circumferencedety
%
% OUTPUT: 
% cartesian coordinates of the circle
    n = size(X, 1);
    best_count = 0;
    best = [];
    for iter = 1:num_iter
        pts = X(randperm(n, 3), :);
        A = [pts, ones(3,1)];
        detA = det(A);
        if abs(detA) < 1e-10
            continue;
        end
        detx = det(-[sum(pts.^2,2), pts(:,2), ones(3,1)]);
        dety = det( [sum(pts.^2,2), pts(:,1), ones(3,1)]);
        c  = det(-[sum(pts.^2,2), pts(:,1:2)]);
        x0_c = -detx / (2 * detA);
        y0_c = -dety / (2 * detA);
        r_c  = sqrt(x0_c^2 + y0_c^2 - c / detA);
        dists = dist(X, x0_c, y0_c, r_c);
        inl = find(abs(dists) <= threshold);
        if numel(inl) > best_count
            best_count = numel(inl);
            best = inl;
        end
    end
    if isempty(best)
        [x0, y0, r] = deal(NaN);
        return;
    end
    [d, e, f] = fit_circle_nhom(X(best, :));
    [x0, y0, r] = quad_to_center(d, e, f);
end
