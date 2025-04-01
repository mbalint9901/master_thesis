suppressPackageStartupMessages({
  library(rstudioapi)
  library(rmarkdown)
  library(stringr)
  library(janitor)
})

## Meta refreshes ----


rstudioapi::jobRunScript(
  path = "1_scrape_scripts/refresh_meta/hvg_meta.R",
  name = "HVG Meta",
  workingDir = getwd()
)

rstudioapi::jobRunScript(
  path = "1_scrape_scripts/refresh_meta/index_meta.R",
  name = "Index Meta",
  workingDir = getwd()
)

rstudioapi::jobRunScript(
  path = "1_scrape_scripts/refresh_meta/vg_meta.R",
  name = "VG Meta",
  workingDir = getwd()
)


## Text refreshes ----

rstudioapi::jobRunScript(
  path = "1_scrape_scripts/refresh_text/hvg_text.R",
  name = "HVG Text",
  workingDir = getwd()
)


rstudioapi::jobRunScript(
  path = "1_scrape_scripts/refresh_text/index_text.R",
  name = "Index Text",
  workingDir = getwd()
)

rstudioapi::jobRunScript(
  path = "1_scrape_scripts/refresh_text/vg_text.R",
  name = "VG Text",
  workingDir = getwd()
)

rstudioapi::jobRunScript(
  path = "1_scrape_scripts/init_text/mtva_text.R",
  name = "MTVA Total",
  workingDir = getwd()
)



## Calculation refreshes ----

rstudioapi::jobRunScript(
  path = "01_script_run.R",
  name = "Data wrangling",
  workingDir = getwd()
)


## Calculation refreshes ----


rstudioapi::jobRunScript(
  path = "02_scaling.R",
  name = "Scaling",
  workingDir = getwd()
)


## Report generating ----

rmarkdown::render(
  "03_results.Rmd",
  output_file = stringr::str_c("report_", janitor::make_clean_names(Sys.Date())),
  output_dir = "2_results/report_pdf"
)
