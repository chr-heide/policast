# Setup
model_out_path <- "forecast/latest_run.RDS"
update_polls <- TRUE
web_dir <- "heide"
run_model <- TRUE
model_script <- "code/01_run_model.R"

# Libraries
library(tidyverse)
library(rstan)
library(stringr)
library(lubridate)
library(gridExtra)
library(pbapply)
library(parallel)
library(boot)
library(lqmm)
library(gridExtra)
library(ggrepel)
library(plotly)
library(ggthemes)
library(tidybayes)
library(maps)
library(quarto)
library(git2r)

# Updating polls if specified
if (update_polls) {
  url <- "https://projects.fivethirtyeight.com/2024-general-data/538_2024_election_forecast_data.zip"
  destfile <- "fivethirtyeight_data"
  download_folder_name <- "538_2024_election_forecast_data"
  filename <- "poll_list.csv"
  download.file(url, destfile)
  unzip(destfile, overwrite = TRUE)
  file.copy(from = paste0(download_folder_name, "/", filename),
            to = filename,
            overwrite = TRUE)
  file.remove(destfile)
}

# Setting up folders etc.
if (!dir.exists("forecast")) {
  dir.create("forecast", recursive = TRUE)
}
if (!dir.exists(web_dir)) {
  dir.create(web_dir, recursive = TRUE)
}
if (!dir.exists(paste0(web_dir, "/web_files"))) {
  dir.create(paste0(web_dir, "web_files"), recursive = TRUE)
}


source(model_script)
source("code/02_postprocessing.R")
source("code/03_visualizations.R")
source("code/04_render_html.R")
source("code/05_update_github.R")