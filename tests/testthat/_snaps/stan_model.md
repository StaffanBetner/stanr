# stan_model errors when neither file nor code given

    Code
      stan_model()
    Condition
      Error in `stan_model()`:
      ! Either 'file' or 'code' must be provided.

# stan_model errors when both file and code given

    Code
      stan_model(file = path, code = "parameters { real x; }")
    Condition
      Error in `stan_model()`:
      ! Provide either 'file' or 'code', not both.

# stan_model errors on missing file

    Code
      stan_model(file = "nonexistent.stan")
    Condition
      Error in `stan_model()`:
      ! File not found: nonexistent.stan

