# output matches expected output for example input

    # A tibble: 309 x 21
        person_id death_date      x      y node_category node_type  prior_offence_t~
            <dbl> <date>      <dbl>  <dbl> <chr>         <fct>      <chr>           
     1    6.54e10 NA         1.57e6 5.18e6 span          home       <NA>            
     2    1.30e11 NA         1.68e6 5.40e6 span          home       <NA>            
     3    4.53e 9 NA         1.72e6 6.04e6 span          home       <NA>            
     4    9.54e10 NA         1.73e6 5.60e6 span          home       <NA>            
     5    1.03e 9 NA         1.79e6 5.50e6 span          home       <NA>            
     6    1.89e11 NA         1.77e6 5.47e6 span          family_im~ <NA>            
     7    1.57e10 NA         1.60e6 5.45e6 span          family_im~ <NA>            
     8    1.03e 9 NA         1.75e6 5.92e6 span          family_im~ <NA>            
     9    4.30e10 NA         2.04e6 5.71e6 span          family_im~ <NA>            
    10    4.54e 9 NA         1.77e6 5.45e6 span          family_im~ <NA>            
    # ... with 299 more rows, and 14 more variables: span_start_date <date>,
    #   span_end_date <date>, event_start_date <date>, event_end_date <date>,
    #   event_start_time <time>, event_end_time <time>, location_type <chr>,
    #   event_date_time_random <dttm>, daypart <fct>, weekpart <fct>, spring <dbl>,
    #   summer <dbl>, autumn <dbl>, winter <dbl>

