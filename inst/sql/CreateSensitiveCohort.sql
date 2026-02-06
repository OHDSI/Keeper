DROP TABLE IF EXISTS #doi_cohort;
DROP TABLE IF EXISTS #treatment_cohort;
DROP TABLE IF EXISTS #symptom_plus_cohort;
	
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
	WHERE concept_set_name = 'doi'
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
