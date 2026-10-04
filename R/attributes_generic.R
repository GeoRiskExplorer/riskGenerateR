# 01 - Generate generic synthetic attributes

attributes_generic <- function(
  x,
  seed = NULL
) {

  # 01 - Setup --------------------------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(x)

  if (n == 0L) {
    x[["record_id"]] <- character()
    x[["category"]] <- character()
    x[["group"]] <- character()
    x[["status"]] <- character()
    x[["value"]] <- numeric()
    x[["count"]] <- integer()
    x[["record_date"]] <- as.Date(character())

    return(x)
  }


  # 02 - Generate categorical attributes -----------------------------------

  category <- sample(
    c(
      "Category A",
      "Category B",
      "Category C",
      "Category D"
    ),
    size = n,
    replace = TRUE
  )

  group <- sample(
    c(
      "Group 1",
      "Group 2",
      "Group 3"
    ),
    size = n,
    replace = TRUE
  )

  status <- sample(
    c(
      "Active",
      "Inactive",
      "Pending",
      "Complete"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.40,
      0.15,
      0.20,
      0.25
    )
  )


  # 03 - Generate numeric attributes ---------------------------------------

  value <- round(
    stats::runif(
      n,
      min = 0,
      max = 100
    ),
    digits = 2
  )

  count <- stats::rpois(
    n,
    lambda = 5
  )


  # 04 - Generate record dates ---------------------------------------------

  date_start <- as.Date("2021-01-01")
  date_end <- as.Date("2025-12-31")

  record_date <- sample(
    seq(
      date_start,
      date_end,
      by = "day"
    ),
    size = n,
    replace = TRUE
  )


  # 05 - Attach generated attributes ---------------------------------------

  x[["record_id"]] <- sprintf(
    "record_%06d",
    seq_len(n)
  )

  x[["category"]] <- category
  x[["group"]] <- group
  x[["status"]] <- status
  x[["value"]] <- value
  x[["count"]] <- count
  x[["record_date"]] <- record_date

  x
}
