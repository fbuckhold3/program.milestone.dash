miles_app()

n_miles_app()


test_result <- try({
  result <- import_and_select_columns("~/Documents/GitHub/IMSLU RDM/Milestone Data/data/MilestoneEvaluationData-2.csv")
  print(head(result))
  print(names(result))
})

if (inherits(test_result, "try-error")) {
  print("Error occurred during import")
} else {
  print("Import successful")
}

