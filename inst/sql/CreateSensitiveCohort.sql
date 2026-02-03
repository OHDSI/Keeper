DROP TABLE IF EXISTS #doi_cohort;
DROP TABLE IF EXISTS #treatment_cohort;
DROP TABLE IF EXISTS #symptom_plus_cohort;
	
-- #doi_cohort
SELECT person_id AS subject_id,
	MIN(condition_start_date) AS cohort_start_date
INTO #doi_cohort
FROM @cdm_database_schema.condition_occurrence
INNER JOIN @cdm_database_schema.concept_ancestor
	ON condition_concept_id = descendant_concept_id
WHERE ancestor_concept_id IN (
	SELECT concept_id
	FROM #concept_sets
	WHERE concept_set_name = 'doi'
)
GROUP BY person_id;
	
-- #treatment_cohort
SELECT subject_id,
	MIN(cohort_start_date) AS cohort_start_date
INTO #treatment_cohort
FROM (
	SELECT person_id AS subject_id,
		drug_exposure_start_date AS cohort_start_date
	FROM @cdm_database_schema.drug_exposure
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON drug_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('drugs')
		AND ratio < 10
	
	UNION ALL
	
	SELECT person_id AS subject_id,
		procedure_date AS cohort_start_date
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('treatmentProcedures')
		AND ratio < 10
	) tmp
WHERE subject_id NOT IN (
	SELECT subject_id
	FROM #doi_cohort
	)
GROUP BY subject_id;
	
-- #symptom_plus_cohort
SELECT subject_id,
	MIN(cohort_start_date) AS cohort_start_date
INTO #symptom_plus_cohort
FROM (
	SELECT person_id AS subject_id,
		condition_start_date AS cohort_start_date
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
		AND ratio < 10
	
	UNION ALL
	
	SELECT person_id AS subject_id,
		observation_date AS cohort_start_date
	FROM @cdm_database_schema.observation
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON observation_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
		AND ratio < 10
	) symptom
INNER JOIN (
	SELECT person_id,
		condition_start_date AS start_date
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('complications')
		AND ratio < 10
		
	UNION ALL
	
	SELECT person_id,
		procedure_date AS start_date
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('diagnosticProcedures')
		AND ratio < 10
		
	UNION ALL
	
	SELECT person_id,
		measurement_date AS start_date
	FROM @cdm_database_schema.measurement
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON measurement_concept_id = descendant_concept_id
	INNER JOIN #concept_ratios
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('measurements')
		AND ratio < 10
	) plus
ON symptom.subject_id = plus.person_id
	AND plus.start_date >= symptom.cohort_start_date
	AND DATEDIFF(DAY, symptom.cohort_start_date, plus.start_date) <= 365
WHERE subject_id NOT IN (SELECT subject_id FROM #doi_cohort)
	AND subject_id NOT IN (SELECT subject_id FROM #treatment_cohort)
GROUP BY subject_id;

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
	FROM #treatment_cohort
	
	UNION ALL
	
	SELECT subject_id, 
		cohort_start_date
	FROM #symptom_plus_cohort
) tmp;

TRUNCATE TABLE #doi_cohort;
TRUNCATE TABLE #treatment_cohort;
TRUNCATE TABLE #symptom_plus_cohort;
DROP TABLE #doi_cohort;
DROP TABLE #treatment_cohort;
DROP TABLE #symptom_plus_cohort;