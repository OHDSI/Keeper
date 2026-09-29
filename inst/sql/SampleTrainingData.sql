DROP TABLE IF EXISTS #sampled_spec_cohort;
DROP TABLE IF EXISTS #target_dist;
DROP TABLE IF EXISTS #unique_negatives;
DROP TABLE IF EXISTS #sampled_sens_cohort;
DROP TABLE IF EXISTS #sampled_training_data;

SELECT subject_id,
    cohort_start_date
INTO #sampled_spec_cohort
FROM (
  SELECT cohort_definition_id,
    subject_id,
    cohort_start_date,
    ROW_NUMBER() OVER (PARTITION BY cohort_definition_id ORDER BY random_id) AS rn
  FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id = @specific_cohort_id
) cohorts
WHERE rn <= @max_cohort_size_for_fitting;

-- Determine prior obs. time and index year distribution in specific cohort sample 
SELECT index_year, 
    FLOOR(LOG(1.0 * DATEDIFF(d, observation_period_start_date, visit_start_date) + 1.0) / LOG(2.0)) AS prior_obs_bin,
    COUNT(*) AS target_count
INTO #target_dist
FROM #sampled_spec_cohort
INNER JOIN @cdm_database_schema.observation_period
  ON subject_id = person_id
    AND cohort_start_date >= observation_period_start_date
    AND cohort_start_date <= observation_period_end_date
GROUP BY index_year, 
    FLOOR(LOG(1.0 * DATEDIFF(d, observation_period_start_date, visit_start_date) + 1.0) / LOG(2.0));

-- Select 1 random visit per negative person
SELECT subject_id,
  cohort_start_date,
  index_year,
  prior_obs_bin
INTO #unique_negatives
FROM (
  SELECT person_id AS subject_id,
    visit_start_date AS cohort_start_date,
    EXTRACT(YEAR FROM visit_start_date) AS index_year, 
    FLOOR(LOG(1.0 * DATEDIFF(d, observation_period_start_date, visit_start_date) + 1.0) / LOG(2.0)) AS prior_obs_bin,
    ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY random_id) AS rn
  FROM @cdm_database_schema.visit_occurrence
  WHERE person_id NOT IN (
    SELECT subject_id 
    FROM @cohort_database_schema.@cohort_table 
    WHERE cohort_definition_id = @sensitive_cohort_id
  )
) tmp
WHERE rn = 1;

-- Sample to meet the required distribution
SELECT negatives.subject_id,
  negatives.cohort_start_date,
INTO #sampled_sens_cohort
FROM (
  SELECT subject_id,
    cohort_start_date,
    index_year,
    priorcohort_start_date
    ROW_NUMBER() OVER (PARTITION BY index_year,prior_obs_bin ORDER BY random_id) AS rn
  FROM #unique_negatives
) negatives
INNER JOIN #target_strata target_strata
    ON negatives.visit_concept_id = target_strata.visit_concept_id
    AND negatives.index_year = target_strata.index_year
    AND negatives.prior_obs_bin = target_strata.prior_obs_bin
WHERE negatives.rn <= target_strata.target_count;

-- Union the samples
SELECT ROW_NUMBER() OVER (ORDER BY subject_id) AS row_id,
  cohort_definition_id,
  subject_id,
  cohort_start_date
INTO #sampled_training_data
FROM (
  SELECT CAST(1 AS INT) AS cohort_definition_id,
    subject_id,
    cohort_start_date
  FROM #sampled_spec_cohort;
  
  UNION ALL
  
  SELECT CAST(0 AS INT) AS cohort_definition_id,
    subject_id,
    cohort_start_date
  FROM #sampled_sens_cohort; 
) tmp;

DROP TABLE #sampled_spec_cohort;
DROP TABLE #target_dist;
DROP TABLE #unique_negatives;
DROP TABLE #sampled_sens_cohort;