UPDATE @cohort_database_schema.@cohort_table
SET probability = 1.0 / (1.0 + EXP(-prediction.z))
FROM (
  SELECT cohort.subject_id,
    intercept.intercept_value + COALESCE(SUM(f.covariate_value * m.beta), 0) AS z
  FROM @cohort_database_schema.@cohort_table cohort
  CROSS JOIN (
    SELECT beta AS intercept_value
    FROM model_table
    WHERE covariate_id = 0
  ) intercept
  LEFT JOIN #covariates covariates
    ON cohort.subject_id = covariates.row_id
  LEFT JOIN #betas betas
    ON covariates.covariate_id = betas.covariate_id 
      AND betas.covariate_id != 0
  GROUP BY cohort.subject_id,
    intercept.intercept_value
  WHERE cohort_definition_id = @sensitive_cohort_id
) prediction
WHERE person_table.subject_id = prediction.subject_id;
