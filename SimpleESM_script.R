### Calculation of SOC (and N) stocks at fixed depth (FD) and Equivalent Soil Mass (ESM) ###

# Authors: Fabien FERCHAUD and Florent CHLEBOWSKI
# Contact: Fabien FERCHAUD (INRAE) - fabien.ferchaud@inrae.fr
# Date: 2022-12-16


# Options -----------------------------------------------------------------

# Name of the input .xlsx file
input_file_name <- "Test_SimpleESM.xlsx"

# Name of the output directory
output_directory_name <- "Example"

# Option for the reference soil mass ("manual" or "auto")
RefM_option <- "manual"

# Option for calculations - elements: one or two elements ("SOC_only" or "SOC_and_N")
E_calc_option <- "SOC_and_N"

# Option for calculations - isotopes: 13C, or 13C and 15N, or not ("13C" or "13C_15N" or "no")
I_calc_option <- "13C_15N"


# Call SimpleESM function -------------------------------------------------

source("SimpleESM_function.R")

SimpleESM(input_file_name, output_directory_name, RefM_option, E_calc_option, I_calc_option)
