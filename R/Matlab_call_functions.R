######################## MATLAB CALL FUNCTIONS


#' Function to download MATLAB files from github
#'
#' @details no input is require. The files are saved in the WD.
#'
#' @export
#'
get_matlab_files_MECC <- function(){

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/R_Copula.m"
                  , destfile = "R_Copula.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_comparison_tr.m"
                  , destfile = "get_comparison_tr.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_copula.m"
                  , destfile = "get_copula.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_copula_distribution.m"
                  , destfile = "get_copula_distribution.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_copula_multipliers.m"
                  , destfile = "get_copula_multipliers.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_distribution_comparison.m"
                  , destfile = "get_distribution_comparison.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_entropy_marginals.m"
                  , destfile = "get_entropy_marginals.m")

    download.file(url = "https://github.com/pedroalencar1/DroughtSDF/tree/master/Matlab/get_return_periods.m"
                  , destfile = "get_return_periods.m")
}

#' function to run MATLAB stript of MECC
#'
#' @param input_data vector with elements in the following order:
#'   d = threshold ratio for transformation (default is 0.01, after Singh and Zhang(2018)
#'   year_separator = indicates the point of breaking between reference na study datasets
#'   export_report = logical - option to export or not a report with informaiton about the copulas
#'   export_copula = logical - option to export or not the cumulative and survival copulas
#'   values_p = a sequence of numbers (until the end of the file) with values in (0,1) of
#'   probabilities of interest to compute the return periods.
#' @param input_series dataframe with 3 columns. Columns are YEARS, X1 (SEVERITY), X2(DURATION)
#'
#' @export
#'
run_matlab_MECC <- function(input_data, input_series){

    write.table(input_data, 'input_data.csv', row.names = F, col.names = F)
    write.csv(input_series, 'input_series.csv', row.names = F)


    if (matlabr::have_matlab()){
        matlabr::get_matlab()
        matlabr::run_matlab_script(fname = 'R_Copula.m')
    }

}
