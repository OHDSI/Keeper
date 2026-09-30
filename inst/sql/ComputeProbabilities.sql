SELECT cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date,
  1.0 / (1.0 + EXP(-prediction.z)) AS probability
INTO #cohort_with_probabilities
FROM (
  SELECT cohort_definition_id,
    cohort.subject_id,
    cohort_start_date,
    cohort_end_date,
    intercept.intercept_value + COALESCE(SUM(covariates.covariate_value * betas.beta), 0) AS z
  FROM @cohort_database_schema.@cohort_table cohort
  CROSS JOIN (
    SELECT beta AS intercept_value
    FROM #betas
    WHERE covariate_id = 0
  ) intercept
  LEFT JOIN #covariates covariates
    ON cohort.subject_id = covariates.row_id
  LEFT JOIN #betas betas
    ON covariates.covariate_id = betas.covariate_id 
      AND betas.covariate_id != 0
  WHERE cohort_definition_id = @sensitive_cohort_id
  GROUP BY cohort_definition_id,
    cohort.subject_id,
    cohort_start_date,
    cohort_end_date,
    intercept.intercept_value
  
) prediction;

DELETE FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @sensitive_cohort_id;

INSERT INTO @cohort_database_schema.@cohort_table
SELECT * FROM #cohort_with_probabilities;

DROP TABLE #cohort_with_probabilities;