% ADDME get a comparison between two datasets of their return period
%
% input data *IN THIS ORDER*
%
% RAW DATA (study): two-column matrix with raw X and Y distributions (severity and duration)
%
% RETURN PERIOD OF REFERENCE: five-column matrix. The output of `get_return_period()` for the
% reference data
%
% D: a threshold ratio for transformation (e.g. 0.01, after Singh and Zhang(2018))
%
% FILENAME: string containing the name of the file (.CSV) where the results will also be exported


function comparison_tr = get_comparison_tr(raw_data_study, tr_reference, d, filename)    

    % get marginal distribution of raw data of study area
    [marg_multipliers, marginals] = get_entropy_marginals(raw_data_study, d);

    % POME-based marginals distributions
    F_u = @(x) integral(@(x) exp(-marg_multipliers(1,1) - marg_multipliers(1,2)*x - ...
        marg_multipliers(1,3)*x.^2),0,x);
    F_v = @(x) integral(@(x) exp(-marg_multipliers(2,1) - marg_multipliers(2,2)*x - ...
        marg_multipliers(2,3)*x.^2),0,x);

    % obtain marginals in the study distribution for the events of interest from reference
    limits = [max(raw_data_study); min(raw_data_study)];
    
    %values of interest (raw)
    x_values = unique(tr_reference(:,3));
    y_values = unique(tr_reference(:,4));

    % values of interest (transformed to between 0 and 1 using scale of study data)
    x_values_transformed =  (x_values - (1-d)*limits(2,1))/((1+d)*limits(1,1) - (1-d)*limits(2,1));
    y_values_transformed =  (y_values - (1-d)*limits(2,2))/((1+d)*limits(1,2) - (1-d)*limits(2,2));

    % marginal values of the transformed values of interest
     % get entropy based marginals
    for i = 1:size(x_values_transformed,1)
        marginal_values(i,1) = F_u(x_values_transformed(i)); % second output - marginals
        marginal_values(i,2) = F_v(y_values_transformed(i)); % second output - marginals
    end

    %get copula multipliers *study*
    copula_multipliers = get_copula_multipliers(marginals);

    % define copula (primitive) *study*
    Copula = @(um, vm) integral2(@(u,v) exp(copula_multipliers(1) - copula_multipliers(2)*u - ...
        copula_multipliers(3)*u.^2 - copula_multipliers(4)*v -  copula_multipliers(5)*v.^2 - ...
        copula_multipliers(6)*u.*v), 0,um, 0, vm);

    values_prob = transpose(combvec(transpose(marginal_values(:,1)), transpose(marginal_values(:,2))));

    for i = 1:size(values_prob, 1)
        values_prob(i,3) = 1/(1 - values_prob(i,1) - values_prob(i,2) + ...
                Copula(values_prob(i,1), values_prob(i,2)));    
    end

    comparison_tr = [tr_reference values_prob(:,3)]; %output

    %export as file
    header = {'u = F(x)', 'v = F(y)', 'x', 'y', 'Tr_ref', 'Tr_new'};
    output = [header; num2cell(comparison_tr)];
    
    % Convert cell to a table and use first row as variable names
    output = cell2table(output(2:end,:),'VariableNames',output(1,:));
    writetable(output,filename);

end