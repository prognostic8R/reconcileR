sum_matrix <- function(data, keys, index, outcomes) {

  data_keys <- data %>%
    dplyr::select(!!!rlang::syms(keys))

  data_keys_1 <- data_keys %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.keys = list({keys}[!is.na(dplyr::cur_data())])) %>%
    dplyr::select(.keys) %>%
    tibble::as_tibble()

  data_keys_2 <- data_keys %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.is_leaf = length(!!keys) == sum(!is.na(dplyr::cur_data()))) %>%
    dplyr::select(.is_leaf) %>%
    tibble::as_tibble()

  data <- dplyr::bind_cols(data_keys_1, data_keys_2, data_keys, data %>% dplyr::select(!!rlang::sym(index), !!!rlang::syms(outcomes)))

  data <- data %>% dplyr::arrange(-.is_leaf)

  rm(data_keys, data_keys_1, data_keys_2)

  data <- data %>%
    dplyr::group_by(.keys) %>%
    dplyr::group_split()

  n_cols_leaf <- nrow(data[[1]])

  sum_matrix_leaf <- diag(n_cols_leaf)
  storage.mode(sum_matrix_leaf) <- "integer"

  sum_matrix_leaf <- setNames(tibble::as_tibble(sum_matrix_leaf), paste0("x", 1:n_cols_leaf))

  data_ <- lapply(data[2:length(data)], function(x) {

    .keys <- x %>% dplyr::select(!!!rlang::syms(.$.keys[[1]]))

    indices <- lapply(seq_along(.keys), function(i) {

      col_name <- names(.keys)[i]
      .key_values <- .keys[[col_name]]

      lapply(seq_along(.key_values), function(j) {

        which(.key_values[j] == data[[1]][[col_name]])
      })
    })

    indices <- lapply(1:nrow(.keys), function(i) {Reduce(intersect, lapply(indices, function(index) {index[[i]]}))})

    sum_matrix <- setNames(tibble::as_tibble(matrix(0L, nrow = nrow(.keys), ncol = n_cols_leaf)), paste0("x", 1:n_cols_leaf))

    for (i in seq_along(indices)) {

      sum_matrix[i, indices[[i]]] <- 1L
    }

    dplyr::bind_cols(x, sum_matrix)
  })

  data <- dplyr::bind_rows(append(data_, list(dplyr::bind_cols(data[[1]], sum_matrix_leaf))))

  return(data)
}
