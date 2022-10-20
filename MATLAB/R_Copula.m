% Pedro Alencar - 17.03.2022  - TU-Berlin
%
% This file is only used to connect R and matlab to run the matlab functions contained in `get_copula`:
% get_entropy_marginals()
% get_copula_multipliers()
% get_copula_distribution()
% get_return_periods()
% get_comparison_tr()
%
% See help of function `get_copula()` for more information about the input file
% output files are generated as csv for visualization in R (see notebook)

get_copula('input_series.csv', 'input_data.csv')

