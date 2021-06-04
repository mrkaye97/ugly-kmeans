  .assign_point_to_centroid <- function(point, centroids) {
    centroids %>%
      map(
        function(x) .calc_dist_to_centroid(point, x)
      ) %>%
      which.min()
  }

  .assign_new_centroids <- function(d, centroids) {
    d %>%
      map(.assign_point_to_centroid, centroids) %>%
      unlist()
  }

  .update_centroids <- function(d, a) {
    d %>%
      bind_rows() %>%
      mutate(assignment = a) %>%
      group_by(assignment) %>%
      summarize(
        across(-contains('assignment'), mean),
        .groups = 'drop'
      ) %>%
      select(-assignment) %>%
      map(as.list) %>%
      transpose()
  }

.calc_dist_to_centroid <- function(p, c) {
    map2_dbl(
      p, c,
      function(x, y) {
        (x-y)**2
      }
    ) %>%
      sum() %>%
      sqrt()
  }


ugly_kmeans <- function(data, k) {
  centroids <- data %>%
    sample_n(k) %>%
    map(as.list) %>%
    transpose()

  ## use lists, because purrr is cool
  data <- data %>%
    as.list() %>%
    transpose()

  ## randomly initialize the initial cluster assignments
  assignments <- sample(1:k, length(data), replace = T)

  old_assignments <- NULL
  current_assignments <- assignments
  i <- 1

  while(!identical(old_assignments, current_assignments)) {
    print(i)

    old_assignments <- current_assignments
    current_assignments <- .assign_new_centroids(data, centroids)

    centroids <- .update_centroids(data, current_assignments)

    i <- i + 1
  }

  current_assignments
}
