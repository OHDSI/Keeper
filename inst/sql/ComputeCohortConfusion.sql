SELECT
  SUM(true_positive) AS true_positives,
  SUM(true_negative) AS true_negatives,
  SUM(false_positive) AS false_positives,
  SUM(false_negative) AS false_negatives,
  SUM(is_case) AS cases,
  COUNT(*) - SUM(is_case) AS non_cases,
  certainty
FROM (
  SELECT CASE WHEN is_case = 1 AND has_match = 1 AND within_window = 1 THEN 1 ELSE 0 END AS true_positive,
    CASE WHEN is_case = 0 AND has_match = 0 THEN 1 ELSE 0 END AS true_negative,
    CASE WHEN is_case = 0 AND has_match = 1 AND within_window = 1 THEN 1 ELSE 0 END AS false_positive,
    CASE WHEN is_case = 1 AND has_match = 0 THEN 1 ELSE 0 END AS false_negative,
    is_case,
    certainty
  FROM (
    SELECT is_case,
      certainty,
      CASE WHEN cohort.subject_id IS NULL THEN 0 ELSE 1 END AS has_match,
      CASE 
        WHEN DATEDIFF(DAY, reference_cohort.cohort_start_date, cohort.cohort_start_date) <= 30
          AND DATEDIFF(DAY, reference_cohort.cohort_start_date, cohort.cohort_start_date) >= -30
        THEN 1 
        ELSE 0
      END AS within_window
    FROM @reference_cohort_database_schema.@reference_cohort_table reference_cohort
    LEFT JOIN @cohort_database_schema.@cohort_table cohort
      ON reference_cohort.subject_id = cohort.subject_id
        AND cohort.cohort_definition_id = @cohort_definition_id
    WHERE reference_cohort.cohort_definition_id = @reference_cohort_definition_id
  ) tmp
) tmp2
GROUP BY certainty;
  
  
