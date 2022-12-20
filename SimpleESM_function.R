### Calculation of SOC (and N) stocks at fixed depth (FD) and Equivalent Soil Mass (ESM) ###

# Authors: Fabien FERCHAUD and Florent CHLEBOWSKI
# Contact: Fabien FERCHAUD (INRAE) - fabien.ferchaud@inrae.fr
# Date: 2022-12-16


SimpleESM <- function(input_file_name, output_directory_name, RefM_option, E_calc_option, I_calc_option) {

  # Packages ----------------------------------------------------------------

  packages <- c("readxl", "tidyr", "dplyr", "foreach", "doParallel")

  # Function to automatically install and/or load packages
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }

  ipak(packages)

  options(dplyr.summarise.inform = FALSE)

  numCores <- detectCores() #Detect the Number of CPU Cores to allow parallel processing


  # 1. Import data -------------------------------------------------------------

  Conc_raw <- read_excel(path = input_file_name, sheet = "Concentrations")

  BD_raw <- read_excel(path = input_file_name, sheet = "BD")

  if(RefM_option == "manual") {
    RefM <- read_excel(path = input_file_name, sheet = "Ref_soil_mass")
  }


  # 2. Check for errors and reshape data ----------------------------------------------------

  is_error <-  FALSE

  ## 2.1. Check for basic errors --------------------------------------------------

  # Inconsistent options
  
  if(E_calc_option == "SOC_only" & I_calc_option == "13C_15N") {
    is_error <-  TRUE
    warning("You can't choose the 13C_15N option with the SOC_only option")
  }
  
  # Missing columns

  if(E_calc_option == "SOC_and_N") {
    if (I_calc_option == "13C_15N") {
      required_colnames_Conc <-  c("Campaign", "Plot", "Point", "Upper_cm", "Lower_cm", "SOC_g_kg", "N_g_kg", "d13C_per1000", "d15N_per1000")
    } else {
      if(I_calc_option == "13C") {
        required_colnames_Conc <-  c("Campaign", "Plot", "Point", "Upper_cm", "Lower_cm", "SOC_g_kg", "N_g_kg", "d13C_per1000")
      } else {
        required_colnames_Conc <-  c("Campaign", "Plot", "Point", "Upper_cm", "Lower_cm", "SOC_g_kg", "N_g_kg")
      }
    }
  } else {
    if (I_calc_option == "13C") {
      required_colnames_Conc <-  c("Campaign", "Plot", "Point", "Upper_cm", "Lower_cm", "SOC_g_kg", "d13C_per1000")
    } else {
      required_colnames_Conc <-  c("Campaign", "Plot", "Point", "Upper_cm", "Lower_cm", "SOC_g_kg")
    }
  }


  if  (!all(required_colnames_Conc %in% colnames(Conc_raw))){
    is_error <-  TRUE
    warning("Missing or misspelled column name(s) in Concentrations")
  }

  required_colnames_BD <-  c("Campaign", "Plot", "Point", "Upper_cm", "Lower_cm", "BD_g_cm3")

  if  (!all(required_colnames_BD %in% colnames(BD_raw))){
    is_error <-  TRUE
    warning("Missing or misspelled column name(s) in BD")
  }

  if(RefM_option == "manual") {
    required_colnames_RefM <-  c("Layer", "Upper_cm", "Lower_cm", "Ref_soil_mass_t_ha")

    if  (!all(required_colnames_RefM %in% colnames(RefM))){
      is_error <-  TRUE
      warning("Missing or misspelled column name(s) in Ref_soil_mass")
    }
  }

  # Missing ID

  if  (any(is.na(Conc_raw$Campaign)  |  is.na(Conc_raw$Plot)  |  is.na(Conc_raw$Point)) | any(is.na(BD_raw$Campaign)  |  is.na(BD_raw$Plot)  |  is.na(BD_raw$Point))){
    is_error <-  TRUE
    warning("Missing Campaign, Plot, or Point values")
  }

  # Missing upper or lower limits

  if  (any(is.na(Conc_raw$Upper_cm)  |  is.na(Conc_raw$Lower_cm) | any(is.na(BD_raw$Upper_cm)  |  is.na(BD_raw$Lower_cm))))    {
    is_error <-  TRUE
    warning("Missing Upper_cm or Lower_cm values")
  }

  if(RefM_option == "manual") {
    if  (any(is.na(RefM$Upper_cm)  |  is.na(RefM$Lower_cm)))    {
      is_error <-  TRUE
      warning("Missing Upper_cm or Lower_cm values")
    }
  }

  # Non-numeric limit values

  if  (any(!is.numeric(Conc_raw$Upper_cm)  |  !is.numeric(Conc_raw$Lower_cm) | !is.numeric(BD_raw$Upper_cm)  |  !is.numeric(BD_raw$Lower_cm)))    {
    is_error <-  TRUE
    warning("Non-numeric Upper_cm or Lower_cm values")
  }

  if(RefM_option == "manual") {
    if  (any(!is.numeric(RefM$Upper_cm)  |  !is.numeric(RefM$Lower_cm)))    {
      is_error <-  TRUE
      warning("Missing Upper_cm or Lower_cm values")
    }
  }

  # Negative limit values

  if  (any(!all(Conc_raw$Upper_cm <= 0)  |  !all(Conc_raw$Lower_cm <= 0) | !all(BD_raw$Upper_cm <= 0) |  !all(BD_raw$Lower_cm <= 0)))    {
    is_error <-  TRUE
    warning("Upper_cm or Lower_cm values must be negative")
  }

  if(RefM_option == "manual") {
    if  (any(!all(RefM$Upper_cm <= 0)  |  !all(RefM$Lower_cm <= 0)))    {
      is_error <-  TRUE
      warning("Upper_cm or Lower_cm values must be negative")
    }
  }

  # Missing Concentrations or BD values

  if(E_calc_option == "SOC_and_N") {
    if(I_calc_option == "13C_15N") {
      if  (any(is.na(Conc_raw$SOC_g_kg) | is.na(Conc_raw$N_g_kg) | is.na(Conc_raw$d13C_per1000) | is.na(Conc_raw$d15N_per1000) | any(is.na(BD_raw$BD_g_cm3)))){
        is_error <-  TRUE
        warning("Missing SOC_g_kg, N_g_kg, d13C_per1000, d15N_per1000 or BD_g_cm3 values")
      }
    } else {
      if(I_calc_option == "13C") {
        if  (any(is.na(Conc_raw$SOC_g_kg) | is.na(Conc_raw$N_g_kg) | is.na(Conc_raw$d13C_per1000) | any(is.na(BD_raw$BD_g_cm3)))){
          is_error <-  TRUE
          warning("Missing SOC_g_kg, N_g_kg, d13C_per1000 or BD_g_cm3 values")
        }
      } else {
        if  (any(is.na(Conc_raw$SOC_g_kg) | is.na(Conc_raw$N_g_kg) | any(is.na(BD_raw$BD_g_cm3)))){
          is_error <-  TRUE
          warning("Missing SOC_g_kg, N_g_kg or BD_g_cm3 values")
        }
      }
    }
  } else {
    if(I_calc_option == "13C") {
      if  (any(is.na(Conc_raw$SOC_g_kg) | is.na(Conc_raw$d13C_per1000) | any(is.na(BD_raw$BD_g_cm3)))){
        is_error <-  TRUE
        warning("Missing SOC_g_kg, d13C_per1000 or BD_g_cm3 values")
      }
    } else {
      if  (any(is.na(Conc_raw$SOC_g_kg) | any(is.na(BD_raw$BD_g_cm3)))){
        is_error <-  TRUE
        warning("Missing SOC_g_kg or BD_g_cm3 values")
      }
    }
  }

  if(RefM_option == "manual") {
    if  (any(is.na(RefM$Ref_soil_mass_t_ha)))    {
      is_error <-  TRUE
      warning("Missing Ref_soil_mass_t_ha values")
    }
  }

  # Non-numeric values

  if(E_calc_option == "SOC_and_N") {
    if(I_calc_option == "13C_15N") {
      if  (any(!is.numeric(Conc_raw$SOC_g_kg)  |  !is.numeric(Conc_raw$N_g_kg)  |  !is.numeric(Conc_raw$d13C_per1000)  |  !is.numeric(Conc_raw$d15N_per1000)  | !is.numeric(BD_raw$BD_g_cm3))){
        is_error <-  TRUE
        warning("Non-numeric SOC_g_kg, N_g_kg, d13C_per1000, d15N_per1000 or BD_g_cm3 values")
      }
    } else {
      if (I_calc_option == "13C") {
        if  (any(!is.numeric(Conc_raw$SOC_g_kg)  |  !is.numeric(Conc_raw$N_g_kg)  |  !is.numeric(Conc_raw$d13C_per1000)  | !is.numeric(BD_raw$BD_g_cm3))){
          is_error <-  TRUE
          warning("Non-numeric SOC_g_kg, N_g_kg, d13C_per1000 or BD_g_cm3 values")
        }
      } else {
        if  (any(!is.numeric(Conc_raw$SOC_g_kg)  |  !is.numeric(Conc_raw$N_g_kg)  | !is.numeric(BD_raw$BD_g_cm3))){
          is_error <-  TRUE
          warning("Non-numeric SOC_g_kg, N_g_kg or BD_g_cm3 values")
        }
      }
    }
  } else {
    if(I_calc_option == "13C") {
      if  (any(!is.numeric(Conc_raw$SOC_g_kg)  |  !is.numeric(BD_raw$BD_g_cm3))){
        is_error <-  TRUE
        warning("Non-numeric SOC_g_kg or BD_g_cm3 values")
      }
    } else {
      if  (any(!is.numeric(Conc_raw$SOC_g_kg) | !is.numeric(Conc_raw$SOC_g_kg)  |  !is.numeric(BD_raw$BD_g_cm3))){
        is_error <-  TRUE
        warning("Non-numeric SOC_g_kg or BD_g_cm3 values")
      }
    }
  }

  if(RefM_option == "manual") {
    if  (any(!is.numeric(RefM$Ref_soil_mass_t_ha)))    {
      is_error <-  TRUE
      warning("Non-numeric Ref_soil_mass_t_ha values")
    }
  }

  # Negative values (or positive values for d13C)

  if(E_calc_option == "SOC_and_N") {
    if(I_calc_option == "13C_15N") {
      if  (any(!all(Conc_raw$SOC_g_kg >=0)  |  !all(Conc_raw$N_g_kg >=0)  |  !all(Conc_raw$d15N_per1000 >=0)  | !all(BD_raw$BD_g_cm3 >=0))){
        is_error <-  TRUE
        warning("SOC_g_kg, N_g_kg, d15N_per1000 or BD_g_cm3 values can't be negative")
      }
      if  (!all(Conc_raw$d13C_per1000 <=0)){
        is_error <-  TRUE
        warning("d13C_per1000 values can't be positive")
      }
    } else {
      if (I_calc_option == "13C") {
        if  (any(!all(Conc_raw$SOC_g_kg >=0)  |  !all(Conc_raw$N_g_kg >=0)  | !all(BD_raw$BD_g_cm3 >=0))){
          is_error <-  TRUE
          warning("SOC_g_kg, N_g_kg or BD_g_cm3 values can't be negative")
        }
        if  (!all(Conc_raw$d13C_per1000 <=0)){
          is_error <-  TRUE
          warning("d13C_per1000 values can't be positive")
        }
      } else {
        if  (any(!all(Conc_raw$SOC_g_kg >=0)  |  !all(Conc_raw$N_g_kg >=0)  | !all(BD_raw$BD_g_cm3 >=0))){
          is_error <-  TRUE
          warning("SOC_g_kg, N_g_kg or BD_g_cm3 values can't be negative")
        }
      }
    }
  } else {
    if(I_calc_option == "13C") {
      if  (any(!all(Conc_raw$SOC_g_kg >=0)  |  !all(BD_raw$BD_g_cm3 >=0))){
        is_error <-  TRUE
        warning("SOC_g_kg or BD_g_cm3 values can't be negative")
      }
    } else {
      if  (any(!all(Conc_raw$SOC_g_kg >=0) | !all(Conc_raw$SOC_g_kg >=0)  |  !all(BD_raw$BD_g_cm3 >=0))){
        is_error <-  TRUE
        warning("SOC_g_kg or BD_g_cm3 values can't be negative")
      }
    }
  }

  if(RefM_option == "manual") {
    if  (any(!all(RefM$Ref_soil_mass_t_ha >=0)))    {
      is_error <-  TRUE
      warning("Ref_soil_mass_t_ha values can't be negative")
    }
  }


  ## 2.2. Convert limits into positive values (in mm) ----------------------------------

  if (is_error == FALSE) {

    Conc_raw <- Conc_raw %>%
      mutate(Upper = round(Upper_cm *(-10), 0),
             Lower = round(Lower_cm *(-10), 0))

    BD_raw <- BD_raw %>%
      mutate(Upper = round(Upper_cm *(-10), 0),
             Lower = round(Lower_cm *(-10), 0))

    if(RefM_option == "manual") {
      RefM <- RefM %>%
        mutate(Upper = round(Upper_cm *(-10), 0),
               Lower = round(Lower_cm *(-10), 0))
    }

  }


  ## 2.3. Add ID columns ---------------------------------------------------

  if (is_error == FALSE) {

    Conc_raw <- Conc_raw %>%
      mutate(
        Point_ID = paste(Campaign, Plot, Point, sep = "_"),
        Sample_ID = paste(Point_ID, Lower, sep="_"))

    BD_raw <- BD_raw %>%
      mutate(Point_ID = paste(Campaign, Plot, Point, sep = "_"),
             Sample_ID = paste(Point_ID, Lower, sep="_"))

  }


  ## 2.4. Average in case of multiple values per sample -----------------------

  if (is_error == FALSE) {

    if(E_calc_option == "SOC_and_N") {
      if(I_calc_option == "13C_15N") {
        Conc <- Conc_raw %>%
          group_by(Campaign, Plot, Point, Upper_cm, Lower_cm, Upper, Lower, Point_ID, Sample_ID) %>%
          summarise(
            SOC_g_kg = mean(SOC_g_kg, na.rm = TRUE),
            N_g_kg = mean(N_g_kg, na.rm = TRUE),
            d13C_per1000 = mean(d13C_per1000, na.rm = TRUE),
            d15N_per1000 = mean(d15N_per1000, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          arrange(Campaign, Plot, Point, Upper, Lower)
      } else {
        if(I_calc_option == "13C") {
          Conc <- Conc_raw %>%
            group_by(Campaign, Plot, Point, Upper_cm, Lower_cm, Upper, Lower, Point_ID, Sample_ID) %>%
            summarise(
              SOC_g_kg = mean(SOC_g_kg, na.rm = TRUE),
              N_g_kg = mean(N_g_kg, na.rm = TRUE),
              d13C_per1000 = mean(d13C_per1000, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            arrange(Campaign, Plot, Point, Upper, Lower)
        } else {
          Conc <- Conc_raw %>%
            group_by(Campaign, Plot, Point, Upper_cm, Lower_cm, Upper, Lower, Point_ID, Sample_ID) %>%
            summarise(
              SOC_g_kg = mean(SOC_g_kg, na.rm = TRUE),
              N_g_kg = mean(N_g_kg, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            arrange(Campaign, Plot, Point, Upper, Lower)
        }
      }

    } else {
      if(I_calc_option == "13C") {
        Conc <- Conc_raw %>%
          group_by(Campaign, Plot, Point, Upper_cm, Lower_cm, Upper, Lower, Point_ID, Sample_ID) %>%
          summarise(
            SOC_g_kg = mean(SOC_g_kg, na.rm = TRUE),
            d13C_per1000 = mean(d13C_per1000, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          arrange(Campaign, Plot, Point, Upper, Lower)
      } else {
        Conc <- Conc_raw %>%
          group_by(Campaign, Plot, Point, Upper_cm, Lower_cm, Upper, Lower, Point_ID, Sample_ID) %>%
          summarise(
            SOC_g_kg = mean(SOC_g_kg, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          arrange(Campaign, Plot, Point, Upper, Lower)
      }
    }

    BD <- BD_raw %>%
      group_by(Campaign, Plot, Point, Upper_cm, Lower_cm, Upper, Lower, Point_ID, Sample_ID) %>%
      summarise(
        BD_g_cm3 = mean(BD_g_cm3, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      arrange(Campaign, Plot, Point, Upper, Lower)

  }


  ## 2.5. Check consistency in and between Conc and BD data --------------------------------------

  if (is_error == FALSE) {

    Conc_limits <- Conc %>%
      group_by(Campaign, Plot, Point) %>%
      summarise(
        Upper = min(Upper),
        Lower = max(Lower)
      ) %>%
      ungroup()

    BD_limits <- BD %>%
      group_by(Campaign, Plot, Point) %>%
      summarise(
        Upper = min(Upper),
        Lower = max(Lower)
      ) %>%
      ungroup()

    Conc_BD_limits <- left_join(Conc_limits, BD_limits, by = c("Campaign", "Plot", "Point")) %>%
      mutate(dif = ifelse(Lower.y < Lower.x, Lower.y - Lower.x, NA))

    rm(Conc_limits, BD_limits)

    if  (any(is.na(Conc_BD_limits$Lower.y))){
      is_error <-  TRUE
      warning("BD data are lacking for at least one sampling point")
    }

    if  (any(!is.na(Conc_BD_limits$dif))){
      is_error <-  TRUE
      warning("Measurement depth for BD must be > or = to sampling depth for concentrations")
    }

    if  (any(Conc_BD_limits$Upper.x > 0 | Conc_BD_limits$Upper.y > 0)){
      is_error <-  TRUE
      warning("All concentrations and BD data must start to 0 cm")
    }

    rm(Conc_BD_limits)

  }


  ## 2.6. Check that all sampled layers are contiguous ---------------------------------

  if (is_error == FALSE) {

    Conc_conti <- Conc %>%
      group_by(Campaign, Plot, Point) %>%
      mutate(Lower_lag = lag(Lower, default = FALSE)) %>%
      mutate(dif = Lower_lag - Upper) %>%
      ungroup()

    if  (any(Conc_conti$dif != 0 )){
      is_error <-  TRUE
      warning("All Conc data must be contiguous")
    }

    BD_conti <- BD %>%
      group_by(Campaign, Plot, Point) %>%
      mutate(Lower_lag = lag(Lower, default = FALSE)) %>%
      mutate(dif = Lower_lag - Upper) %>%
      ungroup()

    if  (any(BD_conti$dif != 0 )){
      is_error <-  TRUE
      warning("All BD data must be contiguous")
    }

    rm(Conc_conti, BD_conti)

  }


  # Final test on the is_error code ---------------------------------------------------

   if  (is_error){
   
     warning("SimpleESM function has failed due to error(s) listed above")
   
   } else {

    # 3. Calculation of soil mass from BD -------------------------------------

    # for BD data

    BD <- BD %>%
      mutate(Soil_mass = (Lower - Upper) * BD_g_cm3 * 10) %>%
      group_by(Campaign, Plot, Point) %>%
      mutate(Soil_mass_cum = cumsum(Soil_mass)) %>%
      ungroup()

    # for Conc data

    Conc <- Conc %>%
      mutate(Soil_mass_cum = 1)

    list_points_Conc <- unique(Conc$Point_ID)

    for (i in 1:length(list_points_Conc)) {

      Conc_i <- dplyr::filter(Conc, Point_ID == list_points_Conc[i])

      BD_i <- dplyr::filter(BD, Point_ID == list_points_Conc[i])

      list_layers_Conc_i <- unique(Conc_i$Lower)

      for (j in 1:length(list_layers_Conc_i)) {

        BD_i_j <- slice(dplyr::filter(BD_i, Lower <= list_layers_Conc_i[j]), n())

        if (nrow(BD_i_j) == 0) {

          BD_i_j_plus1 <- slice(dplyr::filter(BD_i, Lower > list_layers_Conc_i[j]), 1)
          BD_sup <- BD_i_j_plus1$BD_g_cm3

          Conc_i[j,"Soil_mass_cum"] <- list_layers_Conc_i[j] * BD_sup *10

          rm(BD_i_j_plus1, BD_sup)

        } else {

          if (BD_i_j$Lower == list_layers_Conc_i[j]) {

            Conc_i[j,"Soil_mass_cum"] <- BD_i_j$Soil_mass_cum

          } else {

            Depth_inf <- BD_i_j$Lower

            BD_i_j_plus1 <- slice(dplyr::filter(BD_i, Lower > list_layers_Conc_i[j]), 1)

            if (nrow(BD_i_j_plus1)==0) { BD_sup <- BD_i_j$BD_g_cm3 } else { BD_sup <- BD_i_j_plus1$BD_g_cm3 }

            Add_soil_mass <- (list_layers_Conc_i[j] - Depth_inf) * BD_sup *10

            Conc_i[j,"Soil_mass_cum"] <- BD_i_j$Soil_mass_cum + Add_soil_mass

            rm(Depth_inf, BD_i_j_plus1, BD_sup, Add_soil_mass)

          }

        }

      }

      if (i == 1) {
        Conc_mass <- Conc_i
      } else {
        Conc_mass <- bind_rows(Conc_mass, Conc_i)
      }

    }

    Conc <- Conc_mass %>%
      group_by(Campaign, Plot, Point) %>%
      mutate(Soil_mass = Soil_mass_cum - lag(Soil_mass_cum, default = 0)) %>%
      ungroup()

    rm(Conc_i, BD_i, list_layers_Conc_i, BD_i_j, i, j, Conc_mass)

    # In the "auto" option, We search for the sampling point with the lower cumulative soil mass

    if(RefM_option == "auto") {

      min_soil_mass <- Conc %>%
        group_by(Campaign, Plot, Point) %>%
        summarise(Soil_mass_max = max(Soil_mass_cum)) %>%
        ungroup() %>%
        dplyr::filter(Soil_mass_max == min(Soil_mass_max)) %>%
        slice(1)

      Ref_sample_Conc <- Conc %>%
        dplyr::filter(Campaign == min_soil_mass$Campaign, Plot == min_soil_mass$Plot, Point == min_soil_mass$Point)

      RefM <- tibble(
        Layer = seq(1:length(Ref_sample_Conc$Upper)),
        Upper_cm = Ref_sample_Conc$Upper_cm,
        Lower_cm = Ref_sample_Conc$Lower_cm,
        Upper = Ref_sample_Conc$Upper,
        Lower = Ref_sample_Conc$Lower,
        Ref_soil_mass_t_ha = Ref_sample_Conc$Soil_mass
      )

      rm(min_soil_mass)

    }

    list_ref_layers <- unique(RefM$Lower)

    RefM <- mutate(RefM, Ref_soil_mass_cum = cumsum(Ref_soil_mass_t_ha))


    # 4. Calculation of stocks -----------------------------------------------

    
    ## 4.1. Stocks at fixed depths -----------------------------------------

    if (E_calc_option == "SOC_and_N") {
      Stock <- dplyr::select(Conc, Upper_cm:Point_ID, SOC_g_kg:Soil_mass) %>%
        mutate(SOC_stock_FD = Soil_mass * SOC_g_kg /1000) %>%
        mutate(N_stock_FD = Soil_mass * N_g_kg /1000) %>%
        group_by(Point_ID) %>%
        mutate(SOC_stock_cum_FD = cumsum(SOC_stock_FD)) %>%
        mutate(N_stock_cum_FD = cumsum(N_stock_FD)) %>%
        ungroup() %>%
        mutate(C_N_ratio_FD = SOC_stock_FD/N_stock_FD) %>%
        mutate(C_N_ratio_cum_FD = SOC_stock_cum_FD/N_stock_cum_FD)
    } else {
      Stock <- dplyr::select(Conc, Upper_cm:Point_ID, SOC_g_kg:Soil_mass) %>%
        mutate(SOC_stock_FD = Soil_mass * SOC_g_kg /1000) %>%
        group_by(Point_ID) %>%
        mutate(SOC_stock_cum_FD = cumsum(SOC_stock_FD)) %>%
        ungroup()
    }
    
    if(I_calc_option == "13C_15N") {
      Stock <- Stock %>%
        mutate(Q13C = SOC_stock_FD * d13C_per1000) %>%
        mutate(Q15N = N_stock_FD * d15N_per1000) %>%
        group_by(Point_ID) %>%
        mutate(Q13C_cum = cumsum(Q13C)) %>%
        mutate(Q15N_cum = cumsum(Q15N)) %>%
        ungroup() %>%
        mutate(d13C_per1000_cum_FD = Q13C_cum / SOC_stock_cum_FD) %>%
        mutate(d15N_per1000_cum_FD = Q15N_cum / N_stock_cum_FD)
    }
    
    if(I_calc_option == "13C") {
      Stock <- Stock %>%
        mutate(Q13C = SOC_stock_FD * d13C_per1000) %>%
        group_by(Point_ID) %>%
        mutate(Q13C_cum = cumsum(Q13C)) %>%
        ungroup() %>%
        mutate(d13C_per1000_cum_FD = Q13C_cum / SOC_stock_cum_FD)
    }

    # Additional columns

    info_Conc <- Conc_raw %>%
      dplyr::select(Campaign:Point, Point_ID) %>%
      distinct()

    Stock_FD <- left_join(Stock, info_Conc, by = c("Point_ID")) %>%
      group_by(Point_ID) %>%
      mutate(Layer = 1) %>%
      mutate(Layer = cumsum(Layer)) %>%
      ungroup() %>%
      dplyr::select(Campaign:Layer, Upper_cm:Point_ID, everything())

    if (E_calc_option == "SOC_and_N") {
      if(I_calc_option == "13C_15N") {
        Stock_FD <- dplyr::select(Stock_FD, Campaign:Point_ID, Soil_mass_cum, SOC_stock_cum_FD, N_stock_cum_FD, C_N_ratio_cum_FD, d13C_per1000_cum_FD, d15N_per1000_cum_FD, Soil_mass, SOC_stock_FD, N_stock_FD, SOC_g_kg, N_g_kg, C_N_ratio_FD, d13C_per1000, d15N_per1000)
      } else {
        if (I_calc_option == "13C") {
          Stock_FD <- dplyr::select(Stock_FD, Campaign:Point_ID, Soil_mass_cum, SOC_stock_cum_FD, N_stock_cum_FD, C_N_ratio_cum_FD, d13C_per1000_cum_FD, Soil_mass, SOC_stock_FD, N_stock_FD, SOC_g_kg, N_g_kg, C_N_ratio_FD, d13C_per1000)
        } else {
          Stock_FD <- dplyr::select(Stock_FD, Campaign:Point_ID, Soil_mass_cum, SOC_stock_cum_FD, N_stock_cum_FD, C_N_ratio_cum_FD, Soil_mass, SOC_stock_FD, N_stock_FD, SOC_g_kg, N_g_kg, C_N_ratio_FD)
        }
      }
    } else {
      if(I_calc_option == "13C") {
        Stock_FD <- dplyr::select(Stock_FD, Campaign:Point_ID, Soil_mass_cum, SOC_stock_cum_FD, d13C_per1000_cum_FD, Soil_mass, SOC_stock_FD, SOC_g_kg, d13C_per1000)
      } else {
        Stock_FD <- dplyr::select(Stock_FD, Campaign:Point_ID, Soil_mass_cum, SOC_stock_cum_FD, Soil_mass, SOC_stock_FD, SOC_g_kg)
      }
    }

    
    ## 4.2. Stocks at ESM --------------------------------------------------

    # if E_calc_option = "SOC_only", we add a N_g_kg column with 0 values
    if (E_calc_option == "SOC_only") {
      Conc <- mutate(Conc, N_g_kg = 0)
    }

    # if I_calc_option = "no", we add a d13C_per1000 column with 0 values and a d15N_per1000 column with 0 values
    if (I_calc_option == "no") {
      Conc <- mutate(Conc, d13C_per1000 = 0)
      Conc <- mutate(Conc, d15N_per1000 = 0)
    }

    # if I_calc_option = "13C", we add a d15N_per1000 column with 0 values
    if (I_calc_option == "13C") {
      Conc <- mutate(Conc, d15N_per1000 = 0)
    }

    # for loop on sampling points IDs
    cl <- makeCluster(numCores-2)
    registerDoParallel(cl)
    
    Stock_ESM <- foreach(i = 1:(length(list_points_Conc)), .packages=c("tidyr","dplyr"), .combine = rbind) %dopar% {
      
      # Creation of an empty tibble to store calculated values
      Stock_ESM_i <- tibble(
        Point_ID = list_points_Conc[1],
        Layer = 1,
        Lower = 0,
        Soil_mass_cum_ESM = 0,
        SOC_stock_cum_ESM = 0,
        N_stock_cum_ESM = 0,
        Q13C_cum_ESM = 0,
        Q15N_cum_ESM = 0
      )
      Stock_ESM_i <- slice(Stock_ESM_i, -1)
      
      # Conc and BD data for point i
      Conc_i <- dplyr::filter(Conc, Point_ID == list_points_Conc[i])
      BD_i <- dplyr::filter(BD, Point_ID == list_points_Conc[i])
      
      # initialization of depth, cumulative soil mass and cumulative stocks
      Lower_depth_ESM_i <- 0
      Soil_mass_cum_ESM_i <- 0
      SOC_stock_cum_ESM_i <- 0
      N_stock_cum_ESM_i <- 0
      Q13C_cum_ESM_i <- 0
      Q15N_cum_ESM_i <- 0
      
      # for loop on reference layers
      for (j in 1: length(list_ref_layers)) {
        
        # Creation of an tibble to store calculated values
        Stock_ESM_j <- tibble(
          Point_ID = list_points_Conc[1],
          Layer = 1,
          Lower = 0,
          Soil_mass_cum_ESM = 0,
          SOC_stock_cum_ESM = 0,
          N_stock_cum_ESM = 0,
          Q13C_cum_ESM = 0,
          Q15N_cum_ESM = 0
        )
        
        RefM_j <- dplyr::filter(RefM, Lower == list_ref_layers[j])
        
        Soil_mass_ref <- RefM_j$Ref_soil_mass_cum
        
        # while loop to calculate stocks by 1 mm depth increments (elementary layers)
        while(round(Soil_mass_cum_ESM_i, 10) <= Soil_mass_ref) {
          
          Lower_depth_ESM_i <- Lower_depth_ESM_i + 1
          
          # BD, SOC, N and d13C values for the 1 mm elementary layer
          # if the maximal soil sampling depth is lower than the depth needed for ESM, we use the value of the deepest soil sample
          if(Lower_depth_ESM_i <= max(Conc_i$Lower)) {
            BD_i_mm <- dplyr::filter(BD_i, Upper < Lower_depth_ESM_i & Lower >= Lower_depth_ESM_i)$BD_g_cm3
            SOC_i_mm <- dplyr::filter(Conc_i, Upper < Lower_depth_ESM_i & Lower >= Lower_depth_ESM_i)$SOC_g_kg
            N_i_mm <- dplyr::filter(Conc_i, Upper < Lower_depth_ESM_i & Lower >= Lower_depth_ESM_i)$N_g_kg
            d13C_i_mm <- dplyr::filter(Conc_i, Upper < Lower_depth_ESM_i & Lower >= Lower_depth_ESM_i)$d13C_per1000
            d15N_i_mm <- dplyr::filter(Conc_i, Upper < Lower_depth_ESM_i & Lower >= Lower_depth_ESM_i)$d15N_per1000
          } else {
            BD_i_mm <- slice(arrange(BD_i, Lower), n())$BD_g_cm3
            SOC_i_mm <- slice(arrange(Conc_i, Lower), n())$SOC_g_kg
            N_i_mm <- slice(arrange(Conc_i, Lower), n())$N_g_kg
            d13C_i_mm <- slice(arrange(Conc_i, Lower), n())$d13C_per1000
            d15N_i_mm <- slice(arrange(Conc_i, Lower), n())$d15N_per1000
          }
          
          # Calculation of cumulative soil mass and stocks
          Soil_mass_cum_ESM_i <- Soil_mass_cum_ESM_i + BD_i_mm * 10
          SOC_stock_cum_ESM_i <- SOC_stock_cum_ESM_i + BD_i_mm * 10 * SOC_i_mm / 1000
          N_stock_cum_ESM_i <- N_stock_cum_ESM_i + BD_i_mm * 10 * N_i_mm / 1000
          Q13C_cum_ESM_i <- Q13C_cum_ESM_i + BD_i_mm * 10 * SOC_i_mm / 1000 * d13C_i_mm
          Q15N_cum_ESM_i <- Q15N_cum_ESM_i + BD_i_mm * 10 * N_i_mm / 1000 * d15N_i_mm
          
          # end of the while loop
        }
        
        # if we are above the reference soil mass, we subtract 1 mm
        if(round(Soil_mass_cum_ESM_i, 10) > Soil_mass_ref) {
          
          Lower_depth_ESM_i <- Lower_depth_ESM_i - 1
          Soil_mass_cum_ESM_i <- Soil_mass_cum_ESM_i - BD_i_mm * 10
          SOC_stock_cum_ESM_i <- SOC_stock_cum_ESM_i - BD_i_mm * 10 * SOC_i_mm / 1000
          N_stock_cum_ESM_i <- N_stock_cum_ESM_i - BD_i_mm * 10 * N_i_mm / 1000
          Q13C_cum_ESM_i <- Q13C_cum_ESM_i - BD_i_mm * 10 * SOC_i_mm / 1000 * d13C_i_mm
          Q15N_cum_ESM_i <- Q15N_cum_ESM_i - BD_i_mm * 10 * N_i_mm / 1000 * d15N_i_mm
        }
        
        # We store the values into the tibble Stock_ESM_i
        Stock_ESM_j <- Stock_ESM_j %>%
          mutate(
            Point_ID = list_points_Conc[i],
            Layer = j,
            Lower = Lower_depth_ESM_i,
            Soil_mass_cum_ESM = Soil_mass_cum_ESM_i,
            SOC_stock_cum_ESM = SOC_stock_cum_ESM_i,
            N_stock_cum_ESM = N_stock_cum_ESM_i,
            Q13C_cum_ESM = Q13C_cum_ESM_i,
            Q15N_cum_ESM = Q15N_cum_ESM_i
          )
        
        Stock_ESM_i <- bind_rows(Stock_ESM_i, Stock_ESM_j)
        
        # End of the for loop on reference layers
      }
      
      Stock_ESM <- Stock_ESM_i
      
      # End of the for loop on sampling points IDs
    }
    stopCluster(cl)
    
    # just a precaution
    Stock_ESM <- distinct(Stock_ESM)

    # Additional columns

    Stock_ESM <- left_join(Stock_ESM, info_Conc, by = c("Point_ID")) %>%
      group_by(Point_ID) %>%
      mutate(Upper = lag(Lower, default = 0)) %>%
      ungroup() %>%
      mutate(
        Upper_cm = Upper / 10 * -1,
        Lower_cm = Lower / 10 * -1
      ) %>%
      dplyr::select(Campaign:Point, Layer, Upper_cm, Lower_cm, Upper, Lower, everything())

    Stock_ESM <- Stock_ESM %>%
      group_by(Point_ID) %>%
      mutate(Soil_mass_ESM = Soil_mass_cum_ESM - lag(Soil_mass_cum_ESM, default = 0)) %>%
      mutate(SOC_stock_ESM = SOC_stock_cum_ESM - lag(SOC_stock_cum_ESM, default = 0)) %>%
      mutate(SOC_g_kg_ESM = SOC_stock_ESM / Soil_mass_ESM *1000) %>%
      ungroup()

    if (E_calc_option == "SOC_and_N") {
      Stock_ESM <- Stock_ESM %>%
        group_by(Point_ID) %>%
        mutate(N_stock_ESM = N_stock_cum_ESM - lag(N_stock_cum_ESM, default = 0)) %>%
        mutate(N_g_kg_ESM = N_stock_ESM / Soil_mass_ESM *1000) %>%
        mutate(C_N_ratio_ESM = SOC_g_kg_ESM / N_g_kg_ESM) %>%
        mutate(C_N_ratio_cum_ESM = SOC_stock_cum_ESM / N_stock_cum_ESM) %>%
        ungroup()
    }
    
    if (I_calc_option == "13C_15N") {
      Stock_ESM <- Stock_ESM %>%
        mutate(d13C_per1000_cum_ESM = Q13C_cum_ESM / SOC_stock_cum_ESM) %>%
        mutate(d15N_per1000_cum_ESM = Q15N_cum_ESM / N_stock_cum_ESM) %>%
        group_by(Point_ID) %>%
        mutate(Q13C_ESM = Q13C_cum_ESM - lag(Q13C_cum_ESM, default = 0)) %>%
        mutate(d13C_per1000_ESM = Q13C_ESM / SOC_stock_ESM) %>%
        mutate(Q15N_ESM = Q15N_cum_ESM - lag(Q15N_cum_ESM, default = 0)) %>%
        mutate(d15N_per1000_ESM = Q15N_ESM / N_stock_ESM) %>%
        ungroup()
    }

    if (I_calc_option == "13C") {
      Stock_ESM <- Stock_ESM %>%
        mutate(d13C_per1000_cum_ESM = Q13C_cum_ESM / SOC_stock_cum_ESM) %>%
        group_by(Point_ID) %>%
        mutate(Q13C_ESM = Q13C_cum_ESM - lag(Q13C_cum_ESM, default = 0)) %>%
        mutate(d13C_per1000_ESM = Q13C_ESM / SOC_stock_ESM) %>%
        ungroup()
    }


    if (E_calc_option == "SOC_and_N") {
      if(I_calc_option == "13C_15N") {
        Stock_ESM <- Stock_ESM %>%
          dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM, SOC_stock_cum_ESM, N_stock_cum_ESM, C_N_ratio_cum_ESM, d13C_per1000_cum_ESM, d15N_per1000_cum_ESM, Soil_mass_ESM, SOC_stock_ESM, N_stock_ESM, SOC_g_kg_ESM, N_g_kg_ESM, C_N_ratio_ESM, d13C_per1000_ESM, d15N_per1000_ESM)
      } else {
        if (I_calc_option == "13C") {
          Stock_ESM <- Stock_ESM %>%
            dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM, SOC_stock_cum_ESM, N_stock_cum_ESM, C_N_ratio_cum_ESM, d13C_per1000_cum_ESM, Soil_mass_ESM, SOC_stock_ESM, N_stock_ESM, SOC_g_kg_ESM, N_g_kg_ESM, C_N_ratio_ESM, d13C_per1000_ESM)
        } else {
          Stock_ESM <- Stock_ESM %>%
            dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM, SOC_stock_cum_ESM, N_stock_cum_ESM, C_N_ratio_cum_ESM, Soil_mass_ESM, SOC_stock_ESM, N_stock_ESM, SOC_g_kg_ESM, N_g_kg_ESM, C_N_ratio_ESM)
        }
      }
    } else {
      if(I_calc_option == "13C") {
        Stock_ESM <- Stock_ESM %>%
          dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM, SOC_stock_cum_ESM, d13C_per1000_cum_ESM, Soil_mass_ESM, SOC_stock_ESM, SOC_g_kg_ESM, d13C_per1000_ESM)
      } else {
        Stock_ESM <- Stock_ESM %>%
          dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM, SOC_stock_cum_ESM, Soil_mass_ESM, SOC_stock_ESM, SOC_g_kg_ESM)
      }
    }

    
    ## 4.3. Stocks at ESM (cubic spline calculation) ---------------------------

    # if E_calc_option = "SOC_only", we add a N_stock_cum_FD column with 0 values
    if (E_calc_option == "SOC_only") {
      Stock_0 <- mutate(Stock, N_stock_cum_FD = 0)
    } else {
      Stock_0 <- Stock
    }

    # if I_calc_option = "no", we add a d13C_per1000 column with 0 values and a d15N_per1000 column with 0 values
    if (I_calc_option == "no") {
      Stock_0 <- mutate(Stock_0, Q13C_cum = 0)
      Stock_0 <- mutate(Stock_0, Q15N_cum = 0)
    }

    # if I_calc_option = "13C", we add a d15N_per1000 column with 0 values
    if (I_calc_option == "13C") {
      Stock_0 <- mutate(Stock_0, Q15N_cum = 0)
    }

    # Add zero masses at 0 mm (for interpolation)

    Stock_0 <-  dplyr::select(Stock_0, Point_ID, Upper, Lower, Soil_mass_cum, SOC_stock_cum_FD, N_stock_cum_FD, Q13C_cum, Q15N_cum)

    Stock_0 <- Stock_0 %>%
      group_by(Point_ID) %>%
      summarise()  %>%
      mutate(
         Upper = 0,
         Lower = 0,
         Soil_mass_cum=0,
         SOC_stock_cum_FD = 0,
         N_stock_cum_FD = 0,
         Q13C_cum = 0,
         Q15N_cum = 0
         )  %>%
      bind_rows(Stock_0, .)  %>%
      arrange(Point_ID, Upper, Lower) %>%
      ungroup()

    # for loop on sampling points IDs
    for(i in 1: length(list_points_Conc)) {

      # Conc and soil mass data for point i
      Stock_0_i <- dplyr::filter(Stock_0, Point_ID == list_points_Conc[i])

      # Cubic spline interpolation
      SOC_stock_cum_ESM2 <-
      spline(x=Stock_0_i$Soil_mass_cum,
             y=Stock_0_i$SOC_stock_cum_FD,
             xout=RefM$Ref_soil_mass_cum,
             method="hyman")$y

      N_stock_cum_ESM2 <-
        spline(x=Stock_0_i$Soil_mass_cum,
               y=Stock_0_i$N_stock_cum_FD,
               xout=RefM$Ref_soil_mass_cum,
               method="hyman")$y

      Q13C_cum_ESM2 <-
        spline(x=Stock_0_i$Soil_mass_cum,
               y=Stock_0_i$Q13C_cum,
               xout=RefM$Ref_soil_mass_cum,
               method="hyman")$y
      
      Q15N_cum_ESM2 <-
        spline(x=Stock_0_i$Soil_mass_cum,
               y=Stock_0_i$Q15N_cum,
               xout=RefM$Ref_soil_mass_cum,
               method="hyman")$y
      
      Lower <-
        spline(x=Stock_0_i$Soil_mass_cum,
               y=Stock_0_i$Lower,
               xout=RefM$Ref_soil_mass_cum,
               method="hyman")$y

      # Creation of a tibble to store calculated values
      Stock_ESM2_i <- tibble(
        Point_ID = list_points_Conc[i],
        Upper = NA,
        Lower = Lower,
        Soil_mass_cum_ESM2 = RefM$Ref_soil_mass_cum,
        SOC_stock_cum_ESM2 = SOC_stock_cum_ESM2,
        N_stock_cum_ESM2 = N_stock_cum_ESM2,
        Q13C_cum_ESM2 = Q13C_cum_ESM2,
        Q15N_cum_ESM2 = Q15N_cum_ESM2
      ) %>%
        mutate(Upper = lag(Lower, default = 0))

      if (i == 1) {
        Stock_ESM2 <- Stock_ESM2_i
      } else {
        Stock_ESM2 <- bind_rows(Stock_ESM2, Stock_ESM2_i)
      }

      # End of the for loop on sampling points IDs
    }

    rm(i, Stock_0, Stock_0_i, Stock_ESM2_i, SOC_stock_cum_ESM2, N_stock_cum_ESM2, Q13C_cum_ESM2, Q15N_cum_ESM2)

    # Additional columns

    Stock_ESM2 <- left_join(Stock_ESM2, info_Conc, by = c("Point_ID")) %>%
      group_by(Point_ID) %>%
      mutate(Upper = lag(Lower, default = 0)) %>%
      mutate(Layer = 1) %>%
      mutate(Layer = cumsum(Layer)) %>%
      ungroup() %>%
      mutate(
        Upper_cm = Upper / 10 * -1,
        Lower_cm = Lower / 10 * -1
      ) %>%
      dplyr::select(Campaign:Point, Layer, Upper_cm, Lower_cm, Upper, Lower, everything())

    Stock_ESM2 <- Stock_ESM2 %>%
      group_by(Point_ID) %>%
      mutate(Soil_mass_ESM2 = Soil_mass_cum_ESM2 - lag(Soil_mass_cum_ESM2, default = 0)) %>%
      mutate(SOC_stock_ESM2 = SOC_stock_cum_ESM2 - lag(SOC_stock_cum_ESM2, default = 0)) %>%
      mutate(SOC_g_kg_ESM2 = SOC_stock_ESM2 / Soil_mass_ESM2 *1000) %>%
      ungroup()

    if (E_calc_option == "SOC_and_N") {
      Stock_ESM2 <- Stock_ESM2 %>%
        group_by(Point_ID) %>%
        mutate(N_stock_ESM2 = N_stock_cum_ESM2 - lag(N_stock_cum_ESM2, default = 0)) %>%
        mutate(N_g_kg_ESM2 = N_stock_ESM2 / Soil_mass_ESM2 *1000) %>%
        mutate(C_N_ratio_ESM2 = SOC_g_kg_ESM2 / N_g_kg_ESM2) %>%
        mutate(C_N_ratio_cum_ESM2 = SOC_stock_cum_ESM2 / N_stock_cum_ESM2) %>%
        ungroup()
    }

    if (I_calc_option == "13C_15N") {
      Stock_ESM2 <- Stock_ESM2 %>%
        mutate(d13C_per1000_cum_ESM2 = Q13C_cum_ESM2 / SOC_stock_cum_ESM2) %>%
        mutate(d15N_per1000_cum_ESM2 = Q15N_cum_ESM2 / N_stock_cum_ESM2) %>%
        group_by(Point_ID) %>%
        mutate(Q13C_ESM2 = Q13C_cum_ESM2 - lag(Q13C_cum_ESM2, default = 0)) %>%
        mutate(d13C_per1000_ESM2 = Q13C_ESM2 / SOC_stock_ESM2) %>%
        mutate(Q15N_ESM2 = Q15N_cum_ESM2 - lag(Q15N_cum_ESM2, default = 0)) %>%
        mutate(d15N_per1000_ESM2 = Q15N_ESM2 / N_stock_ESM2) %>%
        ungroup()
    }
    
    if (I_calc_option == "13C") {
      Stock_ESM2 <- Stock_ESM2 %>%
        mutate(d13C_per1000_cum_ESM2 = Q13C_cum_ESM2 / SOC_stock_cum_ESM2) %>%
        group_by(Point_ID) %>%
        mutate(Q13C_ESM2 = Q13C_cum_ESM2 - lag(Q13C_cum_ESM2, default = 0)) %>%
        mutate(d13C_per1000_ESM2 = Q13C_ESM2 / SOC_stock_ESM2) %>%
        ungroup()
    }


    if (E_calc_option == "SOC_and_N") {
      if(I_calc_option == "13C_15N") {
        Stock_ESM2 <- Stock_ESM2 %>%
          dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM2, SOC_stock_cum_ESM2, N_stock_cum_ESM2, C_N_ratio_cum_ESM2, d13C_per1000_cum_ESM2, d15N_per1000_cum_ESM2, Soil_mass_ESM2, SOC_stock_ESM2, N_stock_ESM2, SOC_g_kg_ESM2, N_g_kg_ESM2, C_N_ratio_ESM2, d13C_per1000_ESM2, d15N_per1000_ESM2)
      } else {
        if(I_calc_option == "13C") {
          Stock_ESM2 <- Stock_ESM2 %>%
            dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM2, SOC_stock_cum_ESM2, N_stock_cum_ESM2, C_N_ratio_cum_ESM2, d13C_per1000_cum_ESM2, Soil_mass_ESM2, SOC_stock_ESM2, N_stock_ESM2, SOC_g_kg_ESM2, N_g_kg_ESM2, C_N_ratio_ESM2, d13C_per1000_ESM2)
        } else {
          Stock_ESM2 <- Stock_ESM2 %>%
            dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM2, SOC_stock_cum_ESM2, N_stock_cum_ESM2, C_N_ratio_cum_ESM2, Soil_mass_ESM2, SOC_stock_ESM2, N_stock_ESM2, SOC_g_kg_ESM2, N_g_kg_ESM2, C_N_ratio_ESM2)
        }
      }
    } else {
      if(I_calc_option == "13C") {
        Stock_ESM2 <- Stock_ESM2 %>%
          dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM2, SOC_stock_cum_ESM2, d13C_per1000_cum_ESM2, Soil_mass_ESM2, SOC_stock_ESM2, SOC_g_kg_ESM2, d13C_per1000_ESM2)
      } else {
        Stock_ESM2 <- Stock_ESM2 %>%
          dplyr::select(Campaign:Point_ID, Soil_mass_cum_ESM2, SOC_stock_cum_ESM2, Soil_mass_ESM2, SOC_stock_ESM2, SOC_g_kg_ESM2)
      }
    }

    rm(info_Conc)


    
    # 5. Save the files -------------------------------------------------------

    if(dir.exists(output_directory_name) == FALSE) {dir.create(output_directory_name)}

    write.table(Stock_FD, paste(output_directory_name, "Output_FD.csv", sep = "/"), sep = ";", row.names = FALSE, col.names = TRUE)

    write.table(Stock_ESM, paste(output_directory_name, "Output_ESM.csv", sep = "/"), sep = ";", row.names = FALSE, col.names = TRUE)

    write.table(Stock_ESM2, paste(output_directory_name, "Output_ESM2.csv", sep = "/"), sep = ";", row.names = FALSE, col.names = TRUE)

    print(paste("Results have been saved in", paste(getwd(), output_directory_name, sep ="/")))

  }

}
