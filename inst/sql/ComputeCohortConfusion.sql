SELECT SUM(true_positive) AS true_positives,
  SUM(true_negative) AS true_negatives,
  SUM(false_positive) AS false_positives,
  SUM(false_negative) AS false_negatives,
  certainty
FROM (
  SELECT CASE WHEN is_case = 1 AND has_match = 1 AND within_window = 1 THEN 1 ELSE 0 END AS true_positive,
    CASE WHEN is_case = 0 AND has_match = 0 THEN 1 ELSE 0 END AS true_negative,
    CASE WHEN is_case = 0 AND has_match = 1 AND within_window = 1 THEN 1 ELSE 0 END AS false_positive,
    CASE WHEN is_case = 1 AND has_match = 0 THEN 1 ELSE 0 END AS false_negative,
    certainty
  FROM (
    SELECT subject_id,
      is_case,
      certainty,
      MAX(has_match) AS has_match,
      MAX(within_window) AS within_window
    FROM (
      SELECT reference_cohort.subject_id,
        is_case,
        certainty,
        CASE WHEN cohort.subject_id IS NULL THEN 0 ELSE 1 END AS has_match,
  {@type == 'incident'} ? {
        CASE 
          WHEN DATEDIFF(DAY, reference_cohort.cohort_start_date, cohort.cohort_start_date) <= 30
            AND DATEDIFF(DAY, reference_cohort.cohort_start_date, cohort.cohort_start_date) >= -30
          THEN 1 
          ELSE 0
        END AS within_window
  } : {
        1 AS within_window
  }
      FROM @reference_cohort_database_schema.@reference_cohort_table reference_cohort
      LEFT JOIN @cohort_database_schema.@cohort_table cohort
        ON reference_cohort.subject_id = cohort.subject_id
          AND cohort.cohort_definition_id = @cohort_definition_id
          AND DATEDIFF(DAY, observation_period_start_date, cohort.cohort_start_date) >= @washout_period
          AND cohort.cohort_start_date <= observation_period_end_date
      WHERE reference_cohort.cohort_definition_id = @reference_cohort_definition_id
        AND (reference_cohort.cohort_start_date IS NULL OR DATEDIFF(DAY, observation_period_start_date, reference_cohort.cohort_start_date) >= @washout_period)
  {@type == 'incident'} ? {
        AND reference_cohort.cohort_start_date IS NOT NULL
  }
    ) tmp
    GROUP BY subject_id,
      is_case,
      certainty
  ) tmp2
) tmp3
GROUP BY certainty;
  
  
