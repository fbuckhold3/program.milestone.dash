# Function to reorder milestone columns
reorder_milestones <- function(df) {
  milestone_groups <- c("PC", "MK", "SBP", "PBL", "PROF", "ICS")
  cols <- names(df)
  non_milestone_cols <- cols[!grepl("^(PC|MK|SBP|PBL|PROF|ICS)", cols)]
  milestone_cols <- character(0)
  for(group in milestone_groups) {
    pattern <- paste0("^", group)
    group_cols <- sort(cols[grepl(pattern, cols)])
    milestone_cols <- c(milestone_cols, group_cols)
  }
  df <- df[, c(non_milestone_cols, milestone_cols)]
  return(df)
}

import_and_select_columns <- function(file_paths) {
  # Add error handling for file reading
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    tryCatch({
      df <- read.csv(x, stringsAsFactors = FALSE)
      
      # Verify required columns exist
      required_cols <- c("Schedule.Window.Description", "Resident.ID",
                         "Resident.Year", "Question.Key", 
                         "Int.Response.Value")
      
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
      }
      
      # Select only needed columns
      df <- df[, required_cols]
      
      # Clean column names
      colnames(df) <- gsub("\\s+", ".", colnames(df))
      
      return(df)
    }, error = function(e) {
      message("Error reading file ", x, ": ", e$message)
      return(NULL)
    })
  }))
  
  # Remove any NULL entries from failed reads
  combined_data <- combined_data[!sapply(combined_data, is.null)]
  
  if (nrow(combined_data) == 0) {
    stop("No valid data found in the uploaded files")
  }
  
  # Process the data
  processed_data <- combined_data %>%
    mutate(
      Resident.Year = as.character(Resident.Year),
      Int.Response.Value = as.numeric(Int.Response.Value)
    ) %>%
    filter(!is.na(Int.Response.Value)) %>%  # Remove NA values
    filter(!grepl("_followup", Question.Key)) %>%
    mutate(
      Question.Key = case_when(
        grepl("[Cc]omp[1-9]_PC_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", Question.Key),
        grepl("[Cc]omp[1-9]_MK_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", Question.Key),
        grepl("[Cc]omp[1-9]_ICS_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", Question.Key),
        grepl("[Cc]omp[1-9]_SBP_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", Question.Key),
        grepl("[Cc]omp[1-9]_(PROF|PR)_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", Question.Key),
        grepl("[Cc]omp[1-9]_(PBL|PBLI)_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", Question.Key),
        TRUE ~ Question.Key
      )
    ) %>%
    rename(period = Schedule.Window.Description) %>%
    select(Resident.ID, Resident.Year, period, Question.Key, Int.Response.Value) %>%
    group_by(Resident.ID, Resident.Year, period, Question.Key) %>%
    summarise(Int.Response.Value = mean(Int.Response.Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = Question.Key,
      values_from = Int.Response.Value
    ) %>%
    arrange(Resident.ID, Resident.Year)
  
  # Verify that we have milestone columns
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(processed_data), value = TRUE)
  if (length(milestone_cols) == 0) {
    stop("No milestone columns found in the data")
  }
  
  return(processed_data)
}


# Your data preparation function - old one keeping in place just in case for now... (1/29/2025)
import_and_select_columns_old <- function(file_paths) {
  columns_of_interest <- c("Schedule.Window.Description", "First.Name", 
                           "Last.Name", "Resident.Year", "Question.Key", 
                           "Int.Response.Value")
  
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    df <- read.csv(x)
    colnames(df) <- gsub("\\.", ".", colnames(df))
    df[, columns_of_interest]
  }))
  
  combined_data %>%
    mutate(
      name = paste(First.Name, Last.Name),
      Resident.Year = as.character(Resident.Year)
    ) %>%
    filter(!grepl("_followup", Question.Key)) %>%
    mutate(
      Question.Key = gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", Question.Key)
    ) %>%
    rename(period = Schedule.Window.Description) %>%
    select(name, Resident.Year, period, Question.Key, Int.Response.Value) %>%
    pivot_wider(
      names_from = Question.Key,
      values_from = Int.Response.Value,
      values_fn = mean
    ) %>%
    arrange(name, Resident.Year)
}

