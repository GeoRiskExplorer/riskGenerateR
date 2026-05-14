# 01 — Create riskGenerateR package skeleton

usethis::create_package(
  path = "E:/Packages/riskGenerateR",
  open = FALSE
)

usethis::use_mit_license("Robert Andronaco")
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_testthat()
usethis::use_package("sf")
usethis::use_package("dplyr", type = "Suggests")
usethis::use_package("tibble", type = "Suggests")
usethis::use_package("purrr", type = "Suggests")
usethis::use_package("units", type = "Suggests")
usethis::use_package("rlang", type = "Suggests")
usethis::use_package("withr", type = "Suggests")

usethis::use_git()

# 02 — Starter R files

files <- c(
  "R/rg_study_area.R",
  "R/rg_points.R",
  "R/rg_grid.R",
  "R/rg_hex.R",
  "R/rg_polygons.R",
  "R/rg_risk_attributes.R",
  "R/rg_scenario.R",
  "R/rg_check_geometry.R",
  "R/rg_helpers.R"
)

file.create(file.path("E:/Packages/riskGenerateR", files))