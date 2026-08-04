# chain validation rejects invalid chains

    Code
      newstan:::.newstan_validate_chains(chains = 0, chain_ids = integer())
    Condition
      Error:
      ! `chains` must be a single integer >= 1.

# chain validation rejects non-consecutive chain_ids

    Code
      newstan:::.newstan_validate_chains(chains = 2, chain_ids = c(1L, 3L))
    Condition
      Error:
      ! The current backend requires `chain_ids` to be unique consecutive integers.

# seed validation rejects an invalid seed

    Code
      newstan:::.newstan_seed(-1)
    Condition
      Error:
      ! `seed` must be NULL or a single integer between 0 and 2^31 - 1.

