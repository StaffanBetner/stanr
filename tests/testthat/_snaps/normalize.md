# sample normalization rejects invalid chains

    Code
      newstan:::.newstan_normalize_sample(seed = 42L, chains = 0, chain_ids = integer())
    Condition
      Error:
      ! `chains` must be a positive integer.

# sample normalization rejects non-consecutive chain_ids

    Code
      newstan:::.newstan_normalize_sample(seed = 42L, chains = 2, chain_ids = c(1L,
        3L))
    Condition
      Error:
      ! The current backend requires `chain_ids` to be unique consecutive integers.

# laplace normalization rejects mode and opt_args together

    Code
      newstan:::.newstan_normalize_laplace(seed = 42L, mode = c(theta = 0.5),
      opt_args = list(iter = 100L))
    Condition
      Error:
      ! `mode` and `opt_args` cannot both be supplied.

