function p_rating = RWfunc(params, xx)

% initialize parameters
alpha       =params(1);

%initialize data
r = xx(:,1);

% RW model
trials = size(r,1); % establish number of trials
p_rating = zeros(trials+1, 1);

for t=1:(trials-1)
    delta = r(t)-p_rating(t,:);
    p_rating(t+1,:) = p_rating(t,:) + alpha*delta;
end    