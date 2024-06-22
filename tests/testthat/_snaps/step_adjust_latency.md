# printing step_adjust_latency results in expected output

    Code
      r5
    Message
      
      -- Epi Recipe ------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      raw:        2
      geo_value:  1
      time_value: 1
      
      -- Operations 
      1. Lagging: death_rate by 0, 6, 11
      2. Lagging: case_rate by 1, 5
      3. Leading: death_rate by 7
      4. extend_lags: case_rate with latency set at train time

---

    Code
      r
    Message
      
      -- Epi Recipe ------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      raw:        2
      geo_value:  1
      time_value: 1
      
      -- Operations 
      1. Leading: death_rate by 7
      2. extend_ahead: all previous predictors with latency set at train time
      3. Lagging: death_rate by 0, 7, 14

