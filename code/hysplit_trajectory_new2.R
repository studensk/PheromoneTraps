hysplit_trajectory_new2 <- function(lat = 49.263,
                               lon = -123.250,
                               height = 50,
                               duration = 24,
                               days = NULL,
                               daily_hours = 0,
                               direction = "forward",
                               met_type = "reanalysis",
                               vert_motion = 0,
                               model_height = 20000,
                               extended_met = FALSE,
                               config = NULL,
                               ascdata = NULL,
                               traj_name = NULL,
                               binary_path = NULL,
                               met_dir = NULL,
                               exec_dir = NULL,
                               softrun = NULL, # This is not evaluated, yet
                               clean_up = TRUE) {
  
  # If the execution dir isn't specified, use the working directory
  if (is.null(exec_dir)) exec_dir <- getwd()
  
  # If the meteorology dir isn't specified, use the working directory
  if (is.null(met_dir)) met_dir <- getwd()
  
  # Set the path for the `hyts_std` binary file
  binary_path <- 
    set_binary_path(
      binary_path = binary_path,
      binary_name = "hyts_std"
    )
  
  # Get the system type
  system_type <- get_os()
  
  # Generate name of output folder
  if (is.null(traj_name)) {
    folder_name <- paste0("traj-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  } else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  
  if (is.null(config)) {
    config_list <- set_config()
  } else {
    config_list <- config
  }
  
  if (is.null(ascdata)) {
    ascdata_list <- set_ascdata()
  } else {
    ascdata_list <- ascdata
  }
  
  # Modify the default `SETUP.CFG` file when the option for extended
  # meteorology is `TRUE`
  if (isTRUE(extended_met)) {
    
    tm_names <-
      config_list %>%
      names() %>%
      vapply(
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE,
        FUN = function(y) y %>% tidy_grepl("^tm_")
      ) %>%
      which()
    
    config_list[tm_names] <- 1
  }
  
  # Write the config and ascdata lists to files in
  # the `exec` directory
  config_list %>% write_config_list(dir = exec_dir)
  ascdata_list %>% write_ascdata_list(dir = exec_dir)
  
  # Stop function if there are vectors of different
  # length for `lat` and `lon`
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", call. = FALSE)
  }
  
  # Download any necessary meteorological data files
  # and return a vector of the all files required
  met_files <- 
    download_met_files(
      met_type = met_type,
      days = days,
      duration = duration,
      direction = direction,
      met_dir = met_dir
    )
  
  # Generate a tibble of receptor sites
  receptors_tbl <- 
    dplyr::tibble(lat = lat, lon = lon) %>%
    dplyr::group_by(lat, lon) %>% 
    tidyr::expand(height = height) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(receptor = dplyr::row_number()) %>%
    dplyr::select(receptor, dplyr::everything())
  
  # Get vector of receptor indices
  receptors <- seq(nrow(receptors_tbl))
  
  # Create a dataframe for the ensemble
  ensemble_tbl <- dplyr::tibble()
  
  recep_file_path_stack <- c()
  
  # For every set of coordinates, perform a set
  # of model runs
  for (receptor in receptors) {
    
    receptor_vals <- 
      get_receptor_values(
        receptors_tbl = receptors_tbl,
        receptor_i = receptor
      )
    
    receptor_i <- receptor_vals$receptor
    lat_i <- receptor_vals$lat
    lon_i <- receptor_vals$lon
    height_i <- receptor_vals$height
    
    list_run_days <- days %>% as.character()
    
    # Initialize a vector for all filenames at this receptor
    trajectory_files <- c()
    
    # Make loop with all run days
    for (i in seq(list_run_days)) {
      
      # Define starting time parameters
      start_year_GMT <- to_short_year(list_run_days[i])
      start_month_GMT <- to_short_month(list_run_days[i])
      start_day_GMT <- to_short_day(list_run_days[i])
      
      # Sort daily starting hours if given as
      # numeric values
      if (inherits(daily_hours, "numeric")) {
        daily_hours <- formatC(sort(daily_hours), width = 2, flag = 0)
      }
      
      # Make nested loop with daily beginning hours
      for (j in daily_hours) {
        
        start_hour_GMT <- j
        
        if (start_year_GMT > 40) {
          full_year_GMT <- paste0("19", start_year_GMT)
        } else {
          full_year_GMT <- paste0("20", start_year_GMT)
        }
        
        # Construct the output filename string for this
        # model run
        output_filename <-
          get_traj_output_filename(
            traj_name = traj_name,
            site = receptor_i,
            direction = direction,
            year = start_year_GMT,
            month = start_month_GMT,
            day = start_day_GMT,
            hour = start_hour_GMT,
            lat = lat_i,
            lon = lon_i,
            height = height_i,
            duration = duration
          )
        
        trajectory_files <- c(trajectory_files, output_filename)
        
        # Write the CONTROL file
        write_traj_control_file(
          start_year_GMT = start_year_GMT,
          start_month_GMT = start_month_GMT,
          start_day_GMT = start_day_GMT,
          start_hour_GMT = start_hour_GMT,
          lat = lat_i,
          lon = lon_i,
          height = height_i,
          direction = direction,
          duration = duration,
          vert_motion = vert_motion,
          model_height = model_height,
          met_files = met_files,
          output_filename = output_filename,
          system_type = system_type,
          met_dir = met_dir,
          exec_dir = exec_dir
        )
        
        # The CONTROL file is now complete and in the
        # working directory, so, execute the model run
        sys_cmd <- 
          paste0(
            "(cd \"",
            exec_dir,
            "\" && \"",
            binary_path,
            "\" ",
            to_null_dev(system_type = system_type),
            ")"
          )
        
        execute_on_system(sys_cmd, system_type = system_type)
        
      }
    }
    
    recep_file_path <- file.path(exec_dir, receptor_i, folder_name)
    
    recep_file_path_stack <- 
      c(recep_file_path_stack, file.path(exec_dir, receptor_i))
    
    # Create the output folder if it doesn't exist
    if (!dir.exists(recep_file_path)) {
      dir.create(path = recep_file_path, recursive = TRUE)
    }
    
    # Move files into the output folder
    file.copy(
      from = file.path(exec_dir, trajectory_files),
      to = recep_file_path,
      copy.mode = TRUE
    )
    
    unlink(file.path(exec_dir, trajectory_files), force = TRUE)
    
    # Obtain a trajectory data frame
    traj_tbl <-
      trajectory_read(output_folder = recep_file_path) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        receptor = receptor_i,
        lat_i = lat_i,
        lon_i = lon_i,
        height_i = height_i
      )
    
    ensemble_tbl <-
      ensemble_tbl %>%
      dplyr::bind_rows(traj_tbl)
  }
  
  if (clean_up) {
    unlink(file.path(exec_dir, traj_output_files()), force = TRUE)
    unlink(recep_file_path_stack, recursive = TRUE, force = TRUE)
  }
  
  ensemble_tbl <-
    ensemble_tbl %>%
    dplyr::select(-c(year, month, day, hour)) %>%
    dplyr::select(
      receptor,
      hour_along,
      traj_dt,
      lat,
      lon,
      height,
      traj_dt_i,
      lat_i,
      lon_i,
      height_i,
      dplyr::everything()
    ) %>%
    dplyr::group_by(
      receptor, hour_along, traj_dt, traj_dt_i, lat_i, lon_i, height_i) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  if (direction == "forward") {
    
    ensemble_tbl <-
      ensemble_tbl %>%
      dplyr::arrange(receptor, traj_dt_i)
    
  } else {
    
    ensemble_tbl <-
      ensemble_tbl %>%
      dplyr::arrange(receptor, traj_dt_i, dplyr::desc(hour_along))
  }
  
  ensemble_tbl %>%
    dplyr::right_join(
      ensemble_tbl %>%
        dplyr::select(receptor, traj_dt_i, lat_i, lon_i, height_i) %>%
        dplyr::distinct() %>%
        dplyr::mutate(run = dplyr::row_number()),
      by = c("receptor", "traj_dt_i", "lat_i", "lon_i", "height_i")
    ) %>%
    dplyr::select(run, dplyr::everything())
}