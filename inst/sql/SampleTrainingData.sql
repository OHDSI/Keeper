DROP TABLE IF EXISTS #sampled_training_data;

SELECT cohort_definition_id,
  subject_id,
  ROW_NUMBER() OVER (ORDER BY subject_id) AS row_id,
  cohort_start_date
INTO #sampled_training_data
FROM (
  SELECT CAST(1 AS INT) AS cohort_definition_id,
    subject_id,
    cohort_start_date
  FROM (
    SELECT subject_id,
      cohort_start_date,
      ROW_NUMBER() OVER (ORDER BY NEWID()) AS rn
    FROM @cohort_database_schema.@cohort_table
    WHERE cohort_definition_id = @specific_cohort_id
  ) all_specific
  WHERE rn <= @max_cohort_size_for_fitting
  
  UNION ALL
  
  SELECT CAST(0 AS INT) AS cohort_definition_id,
    subject_id,
    cohort_start_date
  FROM (
    SELECT subject_id,
      cohort_start_date,
      ROW_NUMBER() OVER (ORDER BY NEWID()) AS rn
    FROM @cohort_database_schema.@cohort_table
    WHERE cohort_definition_id = @sensitive_cohort_id
      AND subject_id NOT IN (
        SELECT subject_id
        FROM @cohort_database_schema.@cohort_table
        WHERE cohort_definition_id = @specific_cohort_id
      )
  ) all_sens_not_spec
  WHERE rn <= @max_cohort_size_for_fitting
) tmp;
