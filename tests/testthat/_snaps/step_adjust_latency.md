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
      1. Adj. extend_lags: has_role("raw") latency TBD at train time
      2. Lagging: death_rate by 0, 6, 11
      3. Lagging: case_rate by 1, 5
      4. Leading: death_rate by 7

---

    Code
      prep(r5, real_x)
    Message
      
      -- Epi Recipe ------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      raw:        2
      geo_value:  1
      time_value: 1
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      1. Adj. extend_lags: case_rate and death_rate w/ forecast date 2021-07-24 |
      2. Trained
      3. Lagging: death_rate by 5, 11, 16, (lat adj) | Trained
      4. Lagging: case_rate by 6, 10, (lat adj) | Trained
      5. Leading: death_rate by 7 | Trained

---

    Code
      r6
    Message
      
      -- Epi Recipe ------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      raw:        2
      geo_value:  1
      time_value: 1
      
      -- Operations 
      1. Lagging: death_rate by 0, 7, 14
      2. Adj. extend_ahead: has_role("raw") latency TBD at train time
      3. Leading: death_rate by 7

---

    Code
      prep(r6, covid_case_death_rates)
    Message
      
      -- Epi Recipe ------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      raw:        2
      geo_value:  1
      time_value: 1
      
      -- Training information 
      Training data contained 20496 data points and no incomplete rows.
      
      -- Operations 
      1. Lagging: death_rate by 0, 7, 14 | Trained
      2. Adj. extend_ahead: case_rate, ... w/ forecast date 2023-03-10 | Trained
      3. Leading: death_rate by -441, (lat adj) | Trained

