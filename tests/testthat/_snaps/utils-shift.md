# extend_ahead warns in case of extreme adjustment

    Code
      adjust_latency(object, x_adjust_ahead)
    Condition
      Warning:
      ! The ahead has been adjusted by 100, which is questionable for it's `time_type` of day
      i input ahead: 7
      i shifted ahead: 107
      i max_time = 2021-07-19 -> as_of = 2021-10-27
    Output
      [1] 107

