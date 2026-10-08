DROP TABLE IF EXISTS #doi_cohort;
DROP TABLE IF EXISTS #combi_cohort;
DROP TABLE IF EXISTS #sensitive_cohort;
DROP TABLE IF EXISTS #doi_events;
DROP TABLE IF EXISTS #category_events;

DELETE FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_definition_id;
	
-- #doi_cohort
SELECT condition_occurrence.person_id AS subject_id,
	MIN(condition_start_date) AS cohort_start_date
INTO #doi_cohort
FROM @cdm_database_schema.condition_occurrence
INNER JOIN @cdm_database_schema.concept_ancestor
	ON condition_concept_id = descendant_concept_id
INNER JOIN @cdm_database_schema.observation_period
	ON condition_occurrence.person_id = observation_period.person_id
		AND condition_start_date >= observation_period_start_date
		AND condition_start_date <= observation_period_end_date
WHERE ancestor_concept_id IN (
	SELECT concept_id
	FROM #concept_sets
	WHERE concept_set_name = '@doi_set'
)
GROUP BY condition_occurrence.person_id;

-- #combi_cohort
SELECT events.person_id AS subject_id,
	MIN(start_date) AS cohort_start_date
INTO #combi_cohort
FROM (
	SELECT person_id,
		drug_exposure_start_date AS start_date,
		'drugs' AS category
	FROM @cdm_database_schema.drug_exposure
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON drug_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('drugs')
		AND ppv > 0.1
	
	UNION ALL
	
	SELECT person_id,
		procedure_date AS start_date,
		'treatmentProcedures' AS category
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('treatmentProcedures')
		AND ppv > 0.1
	
	UNION ALL
	
	SELECT person_id,
		observation_date AS start_date,
		'symptoms' AS category
	FROM @cdm_database_schema.observation
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON observation_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
		AND ppv > 0.1

	UNION ALL
	
	SELECT person_id,
		condition_start_date AS start_date,
		'symptoms' AS category
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
		AND ppv > 0.1
	
	UNION ALL
	
	SELECT person_id,
		condition_start_date AS start_date,
		'complications' AS category
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('complications')
		AND ppv > 0.1
		
	UNION ALL
	
	SELECT person_id,
		procedure_date AS start_date,
		'diagnosticProcedures' AS category
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('diagnosticProcedures')
		AND ppv > 0.1
		
	UNION ALL
	
	SELECT person_id,
		measurement_date AS start_date,
		'measurements' AS category
	FROM @cdm_database_schema.measurement
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON measurement_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('measurements')
		AND ppv > 0.1
	) events
INNER JOIN @cdm_database_schema.observation_period
	ON events.person_id = observation_period.person_id
		AND start_date >= observation_period_start_date
		AND start_date <= observation_period_end_date
WHERE events.person_id NOT IN (SELECT subject_id FROM #doi_cohort)
GROUP BY events.person_id
HAVING COUNT(DISTINCT category) >= 2;

{!@add_stratification_info} ? {

-- No stratification info requested. Just write sensitive cohort to permanent table:
INSERT INTO @cohort_database_schema.@cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT CAST(@cohort_definition_id AS BIGINT) AS cohort_definition_id,
	subject_id,
	cohort_start_date,
	cohort_start_date AS cohort_end_date
FROM (
	SELECT subject_id, 
		cohort_start_date
	FROM #doi_cohort
	
	UNION ALL
	
	SELECT subject_id, 
		cohort_start_date
	FROM #combi_cohort
) tmp;

} : {

-- Add stratification info. First create a temporary sensitive cohort table
SELECT subject_id,
	cohort_start_date
INTO #sensitive_cohort
FROM (
	SELECT subject_id, 
		cohort_start_date
	FROM #doi_cohort
	
	UNION ALL
	
	SELECT subject_id, 
		cohort_start_date
	FROM #combi_cohort
) tmp;

-- Gather all DOI event weeks within +-365 days of the index date
SELECT condition_occurrence.person_id AS subject_id,
	FLOOR(DATEDIFF(DAY, condition_start_date, cohort_start_date) / 7) AS doi_week
INTO #doi_events
FROM @cdm_database_schema.condition_occurrence
INNER JOIN @cdm_database_schema.concept_ancestor
	ON condition_concept_id = descendant_concept_id
INNER JOIN #sensitive_cohort sens_cohort
	ON condition_occurrence.person_id = sens_cohort.subject_id
WHERE condition_start_date >= DATEADD(DAY, -365, sens_cohort.cohort_start_date)
  AND condition_start_date <= DATEADD(DAY, 365, sens_cohort.cohort_start_date)
  AND ancestor_concept_id IN (
  	SELECT concept_id
  	FROM #concept_sets
  	WHERE concept_set_name = '@doi_set'
  );

-- Gather all supporting category events within +-365 days of the index date
SELECT e.person_id,
	e.category
INTO #category_events
FROM (
	SELECT person_id,
		drug_exposure_start_date AS event_date,
		'drugs' AS category
	FROM @cdm_database_schema.drug_exposure
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON drug_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('drugs')
		AND ppv > 0.1
	
	UNION ALL
	
	SELECT person_id,
		procedure_date AS event_date,
		'treatmentProcedures' AS category
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('treatmentProcedures')
		AND ppv > 0.1
	
	UNION ALL
	
	SELECT person_id,
		observation_date AS event_date,
		'symptoms' AS category
	FROM @cdm_database_schema.observation
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON observation_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
		AND ppv > 0.1

	UNION ALL
	
	SELECT person_id,
		condition_start_date AS event_date,
		'symptoms' AS category
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
		AND ppv > 0.1
	
	UNION ALL
	
	SELECT person_id,
		condition_start_date AS event_date,
		'complications' AS category
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('complications')
		AND ppv > 0.1
		
	UNION ALL
	
	SELECT person_id,
		procedure_date AS event_date,
		'diagnosticProcedures' AS category
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('diagnosticProcedures')
		AND ppv > 0.1
		
	UNION ALL
	
	SELECT person_id,
		measurement_date AS event_date,
		'measurements' AS category
	FROM @cdm_database_schema.measurement
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON measurement_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('measurements')
		AND ppv > 0.1
) e
INNER JOIN #sensitive_cohort sens_cohort
	ON e.person_id = sens_cohort.subject_id
WHERE e.event_date >= DATEADD(day, -365, sens_cohort.cohort_start_date)
  AND e.event_date <= DATEADD(day, 365, sens_cohort.cohort_start_date);

-- Combine sensitive cohort with DoI and distinct category counts and write to table
INSERT INTO @cohort_database_schema.@cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, doi_bin, category_bin)
SELECT CAST(@cohort_definition_id AS BIGINT) AS cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_start_date AS cohort_end_date,
  CASE 
    WHEN category_count < 2 THEN 0
    ELSE 1 
  END AS category_bin,
  CASE 
    WHEN doi_count >= 3 THEN 3
    ELSE doi_count
  END AS doi_bin
FROM (
  SELECT sens_cohort.subject_id,
  	sens_cohort.cohort_start_date,
  	COUNT(DISTINCT c.category) AS category_count,
  	COUNT(DISTINCT d.doi_week) AS doi_count
  FROM  #sensitive_cohort sens_cohort
  LEFT JOIN #category_events c
  	ON sens_cohort.subject_id = c.person_id
  LEFT JOIN #doi_events d
  	ON sens_cohort.subject_id = d.subject_id
  GROUP BY sens_cohort.subject_id,
  	sens_cohort.cohort_start_date
) tmp;

DROP TABLE #doi_events;
DROP TABLE #category_events;
DROP TABLE #sensitive_cohort;
} 
