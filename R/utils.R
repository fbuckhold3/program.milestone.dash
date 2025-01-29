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


# Your data preparation function
import_and_select_columns <- function(file_paths) {
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

