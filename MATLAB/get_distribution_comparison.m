% ADDME function go generate comparison of Tr distributions of the two dets
%
% Inputs - IN THIS ORDER
%
%   data_raw = 3-column matrix with YEAR, X1 (severity) and X2 (duration)
%   year_separator = indicates the point of breaking between reference na study datasets
%   d = threshold ratio for transformation (default is 0.01, after Singh and Zhang(2018)
%   multipliers_marginals_2 = first output from `get_entropy_marginals()` for the study dataset
%   multipliers_copula_2 = output from get_copula_multipliers()` for the study dataset
%   filename = string of a .csv file. Preferably start name with output

function get_distribution_comparison(data_raw, year_separator,d, multipliers_marginals_2, multipliers_copula_2, filename)

% get sets
data_raw_1 = data_raw(data_raw(:,1) <= year_separator,2:3);
data_raw_2 = data_raw(data_raw(:,1) > year_separator,2:3);

% get data from copula 1
data = readmatrix("output_copula_dist_1.csv");

u_p = unique(data(2:100,1)); % remove 0 and 1 to avoind nummeric errors

return_periods_all = get_return_periods(data_raw_1, d, u_p, 'output_return_period_1_all.csv');

% get max and min of data sets
limits_1 = [max(data_raw_1); min(data_raw_1)];
limits_2 = [max(data_raw_2); min(data_raw_2)];

% transform raw values of reference distribution (copula output) into study bounds
f_transf = @(x, i) (x - (1-d)*limits_2(2,i))/((1+d)*limits_2(1,i) - (1-d)*limits_2(2,i));

for i  = 1:2
    return_periods_all(:,i+5) = f_transf(return_periods_all(:,(i+2)),i);
end

%define marginals and copula
F_u_2 = @(x) integral(@(x) exp(-multipliers_marginals_2(1,1) - multipliers_marginals_2(1,2)*x - ...
    multipliers_marginals_2(1,3)*x.^2),0,x);
F_v_2 = @(x) integral(@(x) exp(-multipliers_marginals_2(2,1) - multipliers_marginals_2(2,2)*x - ...
    multipliers_marginals_2(2,3)*x.^2),0,x);

Copula_2 = @(um, vm) integral2(@(u,v) exp(multipliers_copula_2(1) - multipliers_copula_2(2)*u - ...
        multipliers_copula_2(3)*u.^2 - multipliers_copula_2(4)*v -  multipliers_copula_2(5)*v.^2 - ...
        multipliers_copula_2(6)*u.*v), 0,um, 0, vm);

%obtain marginal, copula and return period values.
for i = 1:size(return_periods_all,1)
    return_periods_all(i,8) = F_u_2(return_periods_all(i,6));
    return_periods_all(i,9) = F_v_2(return_periods_all(i,7));
    return_periods_all(i,10) = Copula_2(return_periods_all(i,8),return_periods_all(i,9));
    return_periods_all(i,11) = 1/(1 - return_periods_all(i,8) - return_periods_all(i,9) + ...
        return_periods_all(i,10));
end

 header = {'u = F(x)', 'v = F(y)', 'x', 'y', 'Tr_ref', 'xt_2', 'yt_2', 'u_2', 'v_2', 'C_2', 'Tr_2'};
 output = [header; num2cell(return_periods_all)];
    
 % Convert cell to a table and use first row as variable names
 output = cell2table(output(2:end,:),'VariableNames',output(1,:));
 writetable(output,filename);


