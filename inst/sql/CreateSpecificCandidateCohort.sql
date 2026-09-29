DROP TABLE IF EXISTS #doi_cohort;
DROP TABLE IF EXISTS #category_events;
DROP TABLE IF EXISTS #specific_cohort;

-- 1. Identify index date (first DoI diagnosis) for each person
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

-- 2. Gather all supporting category events within +-365 days of the index date
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
INNER JOIN #doi_cohort d
	ON e.person_id = d.subject_id
WHERE e.event_date >= DATEADD(day, -365, d.cohort_start_date)
  AND e.event_date <= DATEADD(day, 365, d.cohort_start_date);

-- 3. Combine DoI cohort with distinct category counts
SELECT d.subject_id,
	d.cohort_start_date,
	d.cohort_start_date AS cohort_end_date,
	COUNT(DISTINCT c.category) AS category_count
INTO #specific_cohort
FROM #doi_cohort d
LEFT JOIN #category_events c
	ON d.subject_id = c.person_id
GROUP BY d.subject_id,
	d.cohort_start_date;

DROP TABLE #doi_cohort;
DROP TABLE #category_events;
