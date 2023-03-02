#' Create sum matrices for forecast reconciliation
#'
#' @description
#' Simply input a `data.frame` and return a sum matrix that automatically captures
#' a forecast hierarchy.
#'
#' @param data A data frame.
#' @param keys A character vector of column names in `data` that define the groups/forecast hierarchy.
#' @param index The name of a date column.
#' @param outcomes The column name of the outcome being forecasted.
#' @param sort If `sort = TRUE` and `.return = "tibble"`, the returned data frame is sorted by the `keys` columns.
#' @param .return The class of the object returned.
#' @returns If `.return = "matrix"`, a matrix. If `.return = "tibble"`, a data frame with class `tibble`.
#' @examples
#' library(reconcileR)
#' keys <- c("country", "state", "city")
#' index <- "date"
#' outcome <- "widgets"
#'
#' data <- get(data(widgets, package = "reconcileR"))
#'
#' dates <- unique(data[[index]])
#'
#' data <- reconcileR::sum_matrix(
#'   data %>% dplyr::filter(!!rlang::sym(index) == min(!!rlang::sym(index))),
#'   keys,
#'   index,
#'   .return = "tibble"
#' )
#' @export
sum_matrix <- function(data, keys, index, outcomes = NULL, sort = FALSE, .return = c("matrix", "tibble")) {

  .return <- .return[1]

  if (.return == "matrix") sort <- FALSE  # A sorted sum matrix without join keys won't have a usable structure.

  data_keys <- data %>%
    dplyr::select(!!!rlang::syms(keys))

  data_keys_1 <- data_keys %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.keys = list({keys}[!is.na(dplyr::cur_data())])) %>%
    tibble::as_tibble() %>%
    dplyr::select(.keys)

  data_keys_2 <- data_keys %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.is_leaf = length(!!keys) == sum(!is.na(dplyr::cur_data()))) %>%
    tibble::as_tibble() %>%
    dplyr::select(.is_leaf)

  data <- dplyr::bind_cols(data_keys_1, data_keys_2, data_keys, data %>% dplyr::select(tidyselect::any_of(c(index, outcomes))))

  rm(data_keys_1, data_keys_2)

  data <- data %>% dplyr::arrange(-.is_leaf)

  data <- data %>%
    dplyr::group_by(.keys) %>%
    dplyr::group_split()

  n_cols_leaf <- nrow(data[[1]])

  sum_matrix_leaf <- diag(n_cols_leaf)
  storage.mode(sum_matrix_leaf) <- "integer"
  sum_matrix_leaf <- stats::setNames(tibble::as_tibble(sum_matrix_leaf), paste0("x", 1:n_cols_leaf))

  data_ <- lapply(data[2:length(data)], function(x) {

    .keys <- x %>% dplyr::select(!!!rlang::syms(.$.keys[[1]]))

    # If there are no keys present, the time series/forecast has been aggregated at the index level.
    if (length(.keys) == 0) {

      sum_matrix <- tibble::as_tibble(matrix(1L, ncol = n_cols_leaf))

    } else {

      indices <- lapply(seq_along(.keys), function(i) {

        col_name <- names(.keys)[i]
        .key_values <- .keys[[col_name]]

        lapply(seq_along(.key_values), function(j) {

          which(.key_values[j] == data[[1]][[col_name]])
        })
      })

      indices <- lapply(1:nrow(.keys), function(i) {Reduce(intersect, lapply(indices, function(index) {index[[i]]}))})

      sum_matrix <- tibble::as_tibble(matrix(0L, nrow = nrow(.keys), ncol = n_cols_leaf))

      for (i in seq_along(indices)) {

        sum_matrix[i, indices[[i]]] <- 1L
      }
    }

    names(sum_matrix) <- paste0("x", 1:n_cols_leaf)

    dplyr::bind_cols(x, sum_matrix)
  })

  data <- dplyr::bind_rows(append(data_, list(dplyr::bind_cols(data[[1]], sum_matrix_leaf))))

  if (isTRUE(sort)) {

    data <- dplyr::left_join(data_keys, data, by = names(data_keys))

    data <- data %>% dplyr::relocate(.keys, .is_leaf, .before = 1)
  }

  if (.return == "matrix") data <- as.matrix(data[, (ncol(data) - n_cols_leaf + 1):ncol(data)], ncol = n_cols_leaf)

  return(data)
}
