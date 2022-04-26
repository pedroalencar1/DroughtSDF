% ADDME function to obtain most entropic canonical copula parameters and distribution 
% Pedro Alencar - 16.03.2022  - TU-Berlin
%
% This application is destined to obtain the MECC of drought events and compare the return periods
% of two distinct intervals (reference and study)
%
% References: example 8.1 from Copulas and their Applications in Water Resources Engineering
%             Singh and Zhang (2018) DOI: 10.1186/s40562-018-0105-z
% solution of potential entropy for Lagrangian multipliers
% E(UV) = (\rho_{spearman}+3)/12
%
% Inputs - in this order
% file_input_series = a csv with 3 columns, exported from R. Columns are YEARS, X1 (SEVERITY), X2(DURATION)
% file_input_data = a csv with a single column. Elements are, in this order:
%   d = threshold ratio for transformation (default is 0.01, after Singh and Zhang(2018)
%   year_separator = indicates the point of breaking between reference na study datasets
%   export_report = logical - option to export or not a report with informaiton about the copulas
%   export_copula = logical - option to export or not the cumulative and survival copulas
%   values_p = a sequence of numbers (until the end of the file) with values in (0,1) of
%   probabilities of interest to compute the return periods.


function get_copula(file_input_series, file_input_data)

%file_input_series = 'input_series.csv';
%file_input_data = 'input_data.csv';

    %read input series 
    data = readtable(file_input_series);
    names = data.Properties.VariableNames;
    data_raw = data{:,:}; % convert table to matrix
    
    %read input data
    input = readtable(file_input_data);
    input_raw = input{:,:};

    %set input data
    d = input_raw(1);
    year_separator = input_raw(2);
    export_report = input_raw(3);
    export_copula = input_raw(4);
    values_p = input_raw(5:size(input_raw,1));
        
    % separate data into reference and study set
    data_raw_1 = data_raw(data_raw(:,1) <= year_separator,2:3);
    data_raw_2 = data_raw(data_raw(:,1) > year_separator,2:3);
    
    % get marginals and their lagrange multipliers
    [multipliers_marginals_1, marginals_1] = get_entropy_marginals(data_raw_1, d);
    [multipliers_marginals_2, marginals_2] = get_entropy_marginals(data_raw_2, d);
    
    %get copula multipliers
    multipliers_copula_1 = get_copula_multipliers(marginals_1);
    multipliers_copula_2 = get_copula_multipliers(marginals_2);
    
    % get distribution of copula and survive copula
    if export_copula == true
        get_copula_distribution(multipliers_copula_1, "output_copula_dist_1.csv");
        get_copula_distribution(multipliers_copula_2, "output_copula_dist_2.csv");
        get_distribution_comparison(data_raw, year_separator,d, multipliers_marginals_2, ...
            multipliers_copula_2, "output_comparison_copula_Tr.csv")
    end
    
    % get Period of Return from specific values
    
    return_periods_1 = get_return_periods(data_raw_1, d, values_p, 'output_return_period_1.csv');
    return_periods_2 = get_return_periods(data_raw_2, d, values_p, 'output_return_period_2.csv');
    
    comparison_1_2 = get_comparison_tr(data_raw_2, return_periods_1, d, 'output_comparison.csv');
    
    %%%% 
    % create report using `diary` function.
    
    if export_report == true
        if exist('output_report.txt', 'file')==2
            delete('output_report.txt');
        end

        diary output_report.txt
            
        formatSpec = '%.3f';
        disp('***************************************************')
        disp('Most Entropic Canonical Copula calibration')
        disp('by Pedro Alencar - 16.03.2022')
        disp('TU-Berlin - FG Ökohydrologie & Landschaftsbewertung')
        disp('***************************************************')
        disp(' ')
        disp('0. Data description')
        disp(' ')
        x1 = ['The data series has: ', num2str(size(data,1)), ' years and was divided into two intervals'];
        disp(x1)
        x2 = ['Interval 1: Reference -- ', num2str(size(data_raw_1,1)), ' year. From ', ...
            num2str(min(data_raw(:,1))), ' to ', num2str(year_separator), '.'];
        disp(x2)
        x3 = ['Interval 1: Reference -- ', num2str(size(data_raw_2,1)), ' year. From ', ...
            num2str(year_separator+1), ' to ', num2str(max(data_raw(:,1))), '.'];
        disp(x3)
        disp(' ')
        disp(' ')
        disp('1. Reference set')
        disp(' ')
        disp('Lagrange multipliers of POME-based marginal distribution')
        disp('var   l0    l1    l2')
        x1 = ['x1:   ', num2str(multipliers_marginals_1(1,:),formatSpec)];
        disp(x1)
        x2 = ['x2:   ', num2str(multipliers_marginals_1(2,:),formatSpec)];
        disp(x2)
        disp(' ')
        disp('**The marginal distribution has the form:')
        disp('u = f(x) = exp(-l0 - l1*x - l2*x^2)')
        disp(' ')
        disp('Lagrange multipliers of MEC-Copula')
        disp(' ')
        disp('Cop  l0     l1    l2      g1    g2     l3')
        x3 = ['c:   ', num2str(multipliers_copula_1,formatSpec)];
        disp(x3)
        disp(' ')
        disp('**The copula function has the form:')
        disp('c(u1, u2) = exp(-l0 - l1*u1 - l2*u1^2 - g1*u2 - g2*u2^2 - l3*u1*u2)')
        disp(' ')
        disp(' ')
        disp('2. Study set')
        disp(' ')
        disp('Lagrange multipliers of POME-based marginal distribution')
        disp('var   l0    l1    l2')
        x1 = ['x1:   ', num2str(multipliers_marginals_2(1,:),formatSpec)];
        disp(x1)
        x2 = ['x2:   ', num2str(multipliers_marginals_2(2,:),formatSpec)];
        disp(x2)
        disp(' ')
        disp('**The marginal distribution has the form:')
        disp('u = f(x) = exp(-l0 - l1*x - l2*x^2)')
        disp(' ')
        disp('Lagrange multipliers of MEC-Copula')
        disp(' ')
        disp('Cop  l0     l1    l2      g1    g2     l3')
        x3 = ['c:   ', num2str(multipliers_copula_2,formatSpec)];
        disp(x3)
        disp(' ')
        disp('**The copula function has the form:')
        disp('c(u1, u2) = exp(-l0 - l1*u1 - l2*u1^2 - g1*u2 - g2*u2^2 - l3*u1*u2)')
        disp(' ')
    
        diary off    
    end

    disp('Done -- check output files')
end
