library(quarto)


file_list <- c(
  "overall_forecast.qmd",
  "forecast_change.qmd",
  "state_results.qmd"
)

for (f in file_list) {
  quarto_render(paste0(web_dir, "/", f))
}