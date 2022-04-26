% function to obtain the entropy-based marginals based on observed data and
% the principle of maximum entropy.

function [marg_multipliers, marginals] = get_entropy_marginals(data_raw, d)

    N = size(data_raw,1); %length of dataset

    data = zeros(size(data_raw), "double");
    % transform data into 0 to 1 interval
    for i = 1:2
    data(:,i) = (data_raw(:,i) - (1-d)*min(data_raw(:,i)))/((1+d)*max(data_raw(:,i)) - ...
        (1-d)*min(data_raw(:,i)));
    end

    % get entropy-based marginal distribution
    % X
    u_t = data(:,1);
    c_u_1 = mean(u_t);
    c_u_2 = mean(u_t.^2);

    int_term_u = @(a) integral(@(u) exp(-a(1)*(u - c_u_1) - a(2)*(u.^2 - c_u_2)),0,1);
    a0_u = [-1,1];
    obj_fun_u = @(a) int_term_u(a);
    a_sol_u = fminsearch(obj_fun_u, a0_u);
    l0_u = log(integral(@(u) exp(-a_sol_u(1)*u - a_sol_u(2)*u.^2),0,1));
    a_sol_u = [l0_u a_sol_u];

    %Y
    v_t = data(:,2);
    c_v_1 = mean(v_t);
    c_v_2 = mean(v_t.^2);

    int_term_v = @(a) integral(@(v) exp(-a(1)*(v - c_v_1) - a(2)*(v.^2 - c_v_2)),0,1);
    a0_v = [-1,1];
    obj_fun_v = @(a) int_term_v(a);
    a_sol_v = fminsearch(obj_fun_v, a0_v);
    l0_v = log(integral(@(v) exp(-a_sol_v(1)*v - a_sol_v(2)*v.^2),0,1));
    a_sol_v = [l0_v a_sol_v];

    marg_multipliers = [a_sol_u;a_sol_v]; % first output - lagrange multipliers

    F_u = @(x) integral(@(x) exp(-marg_multipliers(1,1) - ...
        marg_multipliers(1,2)*x - marg_multipliers(1,3)*x.^2),0,x);
    F_v = @(x) integral(@(x) exp(-marg_multipliers(2,1) - ...
        marg_multipliers(2,2)*x - marg_multipliers(2,3)*x.^2),0,x);

    % get entropy based marginals
    marginals = zeros(size(data), "double");
    for i = 1:N
        marginals(i,1) = F_u(data(i,1)); % second output - marginals
        marginals(i,2) = F_v(data(i,2)); % second output - marginals
    end
end
