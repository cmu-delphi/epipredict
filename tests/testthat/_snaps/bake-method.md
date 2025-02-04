# bake method works in all cases

    Code
      bake(prep(r, edf), NULL, composition = "matrix")
    Condition
      Error in `juice()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "geo_value" and "time_value".

