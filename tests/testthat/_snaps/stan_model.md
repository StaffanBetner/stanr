# stan_model errors when neither file nor code given

    Code
      stan_model()
    Condition
      Error:
      ! Supply exactly one of `stan_file` and `code`.

# stan_model errors when both file and code given

    Code
      stan_model(stan_file = path, code = "parameters { real x; }")
    Condition
      Error:
      ! Supply exactly one of `stan_file` and `code`.

# stan_model errors on missing file

    Code
      stan_model(stan_file = "nonexistent.stan")
    Condition
      Error:
      ! `stan_file` must name an existing Stan file.

