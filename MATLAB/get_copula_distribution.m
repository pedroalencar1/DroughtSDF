function get_copula_distribution(multipliers, filename)

    %define copula function (primitive/cumulative)
    Copula = @(um, vm) integral2(@(u,v) exp(multipliers(1) - multipliers(2)*u - multipliers(3)*u.^2 ...
        - multipliers(4)*v -  multipliers(5)*v.^2 - multipliers(6)*u.*v), 0,um, 0, vm);
    
    %define numerical space (interval) of calculation. Used sqrt distribution
    %of points
    ui = [0:100].^0.5 / 10;
    vj = ui;
    C_values = transpose(combvec(ui, vj));
    
    % compute cumulative for all space
    C_dist = zeros(size(C_values,1),1, "double");
    C_surv = zeros(size(C_values,1),1, "double");
    for (i = 1:size(C_values,1))
    
        C_dist(i) = Copula(C_values(i,1), C_values(i,2));% CDF
        C_surv(i) = -1 + C_values(i,1) + C_values(i,2) + Copula(1-C_values(i,1), ...
            1-C_values(i,2)); % Survival Copula
    
    end
    
    % join to main dataframe for export and add header
    C_values = [C_values C_dist C_surv];
    header = {'u', 'v', 'Cumulative', 'Survival'};
    output = [header; num2cell(C_values)];
    
    % Convert cell to a table and use first row as variable names
    output = cell2table(output(2:end,:),'VariableNames',output(1,:));
    writetable(output,filename);
end