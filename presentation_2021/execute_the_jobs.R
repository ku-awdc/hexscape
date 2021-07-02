scenarios <- c("dk_ddep_720",
               "dk_no_ddep_720",
               "dk_ddep_20",
               "dk_no_ddep_20")
# scenarios <- c(
#   "original_dk_ddep_1720",
#                "original_dk_no_ddep_1720",
#                "original_dk_ddep_20",
#                "original_dk_no_ddep_20")

for (scenario_name in scenarios) {
  job::job({
    cat(scenario_name)
    source(file = "presentation_2021/animations.R", echo = TRUE, local = TRUE)
  }, title = scenario_name)

  job::export(NULL)
}
