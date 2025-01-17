# bake method works in all cases

    Code
      bake(prep(r, edf), NULL, composition = "matrix")
    Condition
      Error in `hardhat::recompose()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "geo_value", "time_value", and ".target_time_value".

