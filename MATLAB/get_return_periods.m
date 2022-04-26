% ADDME get Period of Return from specific values
%
% input data *IN THIS ORDER*
%
% RAW DATA: two-column matrix with raw X and Y distributions (severity and duration)
%
% D: a threshold ratio for transformation (e.g. 0.01, after Singh and Zhang(2018))
% 
% PROBABILITIES: a one-column matrix with probabilities of interest (e.g. [0.8; 0.9; 0.95; 0.98])
%
% FILENAME: string containing the name of the file (.CSV) where the results will also be exported

function return_periods = get_return_periods(data_raw, d, probabilities, filename)

    [multipliers_marginals, marginals] = get_entropy_marginals(data_raw, d);
    multipliers_copula = get_copula_multipliers(marginals);

    % POME-based marginals distributions
    F_u = @(x) integral(@(x) exp(-multipliers_marginals(1,1) - multipliers_marginals(1,2)*x - ...
        multipliers_marginals(1,3)*x.^2),0,x);
    F_v = @(x) integral(@(x) exp(-multipliers_marginals(2,1) - multipliers_marginals(2,2)*x - ...
        multipliers_marginals(2,3)*x.^2),0,x);
        
    n_probs = size(probabilities,1); %number of probabilities of interest
    
    prob_marginals = zeros(n_probs,"double");
    % get marginals and 
    x_0 = 0.5;
    for i  = 1:n_probs
        obj_fun_u = @(a) abs(F_u(a) - probabilities(i));
        a_sol_u = fminsearch(obj_fun_u, x_0);
    
        obj_fun_v = @(a) abs(F_v(a) - probabilities(i));
        a_sol_v = fminsearch(obj_fun_v, x_0);
    
        prob_marginals(i, 1:3) = [probabilities(i) a_sol_u a_sol_v] ;
    end
    
    %min and max from raw data
    limits = [max(data_raw); min(data_raw)];
    
    %get raw marginals
    f_raw = @(xt,i) (1+d)*xt.*limits(1,i) + (1-d)*limits(2,i)*(1-xt);
    for i  = 1:2
        prob_marginals(:,i+3) = f_raw(prob_marginals(:,(i+1)),i);
    end
    
    % define copula (primitive)
    Copula = @(um, vm) integral2(@(u,v) exp(multipliers_copula(1) - multipliers_copula(2)*u - ...
        multipliers_copula(3)*u.^2 - multipliers_copula(4)*v -  multipliers_copula(5)*v.^2 - ...
        multipliers_copula(6)*u.*v), 0,um, 0, vm);
    
    
    values_prob = transpose(combvec(transpose(probabilities), transpose(probabilities)));
    values_raw = transpose(combvec(transpose(prob_marginals(:,4)), transpose(prob_marginals(:,5))));
    values_output = [values_prob values_raw];
    
    for i = 1:size(values_output,1)
        values_output(i,5) = 1/(1 - values_output(i,1) - values_output(i,2) + ...
                Copula(values_output(i,1), values_output(i,2)));
    end
    
    return_periods = values_output; %output

    %export as file
    header = {'u = F(x)', 'v = F(y)', 'x', 'y', 'Tr(x,y)'};
    output = [header; num2cell(return_periods)];
    
    % Convert cell to a table and use first row as variable names
    output = cell2table(output(2:end,:),'VariableNames',output(1,:));
    writetable(output,filename);

end
