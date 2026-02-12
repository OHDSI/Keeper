DROP TABLE IF EXISTS #full_concept_sets;
DROP TABLE IF EXISTS #demographics;
DROP TABLE IF EXISTS #presentation;
DROP TABLE IF EXISTS #visit;
DROP TABLE IF EXISTS #symptoms;
DROP TABLE IF EXISTS #prior_disease;
DROP TABLE IF EXISTS #post_disease;
DROP TABLE IF EXISTS #prior_drugs;
DROP TABLE IF EXISTS #post_drugs;
DROP TABLE IF EXISTS #prior_treatment_procedures;
DROP TABLE IF EXISTS #post_treatment_procedures;
DROP TABLE IF EXISTS #alternative_diagnoses;
DROP TABLE IF EXISTS #diagnostic_procedures;
DROP TABLE IF EXISTS #measurements;
DROP TABLE IF EXISTS #death;

{@use_descendants} ? {
SELECT descendant_concept_id AS concept_id,
	concept_set_name,
	CASE
	  WHEN descendant_concept_id IN (
	    SELECT DISTINCT descendant_concept_id
	    FROM #concept_sets
      INNER JOIN @cdm_database_schema.concept_ancestor
	      ON concept_id = ancestor_concept_id
	    WHERE concept_set_name = 'doi'
	  ) THEN 1
	  WHEN MIN(target) = 0 AND MAX(target) = 1 THEN 2
	  ELSE MAX(target)
	END AS target
INTO #full_concept_sets
FROM #concept_sets
INNER JOIN @cdm_database_schema.concept_ancestor
	ON concept_id = ancestor_concept_id
GROUP BY descendant_concept_id,
	concept_set_name;
} : {
SELECT concept_id,
	concept_set_name,
	CASE
	  WHEN MIN(target) = 0 AND MAX(target) = 1 THEN 2
	  ELSE MAX(target)
	END AS target
INTO #full_concept_sets
FROM #concept_sets
GROUP BY concept_id,
	concept_set_name;
}

-- Demographics
SELECT CAST(subject_id AS VARCHAR) AS person_id,
	generated_id,
	FLOOR(DATEDIFF(DAY, DATEFROMPARTS(year_of_birth, COALESCE(month_of_birth, 1), COALESCE(day_of_birth, 1)), cohort_start_date) / 365.25) AS age,
	gender_concept_id,
	gender_concept.concept_name AS gender_concept_name,
	DATEDIFF(DAY, cohort_start_date, observation_period_start_date) AS observation_start_day,
	DATEDIFF(DAY, cohort_start_date, observation_period_end_date) AS observation_end_day,
	race_concept_id,
	CASE WHEN race_concept.concept_name IS NULL THEN '' ELSE race_concept.concept_name END AS race_concept_name,
	ethnicity_concept_id,
	CASE WHEN ethnicity_concept.concept_name IS NULL THEN '' ELSE ethnicity_concept.concept_name END AS ethnicity_concept_name,
	1 AS target
INTO #demographics
FROM #cohort cohort
INNER JOIN @cdm_database_schema.person
	ON person.person_id = cohort.subject_id
INNER JOIN @cdm_database_schema.observation_period
	ON observation_period.person_id = cohort.subject_id
		AND observation_period_start_date <= cohort_start_date
		AND observation_period_end_date >= cohort_start_date
INNER JOIN @cdm_database_schema.concept gender_concept
	ON gender_concept.concept_id = gender_concept_id
LEFT JOIN @cdm_database_schema.concept race_concept
	ON race_concept.concept_id = race_concept_id
		AND race_concept_id != 0
LEFT JOIN @cdm_database_schema.concept ethnicity_concept
	ON ethnicity_concept.concept_id = ethnicity_concept_id
		AND ethnicity_concept_id != 0;
		
-- Presentation
SELECT generated_id,
	concept_id,
	concept_name,
	extra_data,
	MAX(COALESCE(target, -1)) AS target
INTO #presentation
FROM (
	SELECT generated_id,
		concept.concept_id,
		concept.concept_name,	
		CASE 
			WHEN type_concept.concept_name IS NOT NULL AND status_concept.concept_name IS NOT NULL THEN CONCAT(type_concept.concept_name, ', ', status_concept.concept_name)
			WHEN type_concept.concept_name IS NOT NULL THEN type_concept.concept_name
			WHEN status_concept.concept_name IS NOT NULL THEN status_concept.concept_name
			ELSE ''
		END AS extra_data,
		target
	FROM #cohort cohort
	INNER JOIN @cdm_database_schema.condition_occurrence
		ON condition_occurrence.person_id = cohort.subject_id
			AND condition_start_date = cohort_start_date
	INNER JOIN @cdm_database_schema.concept 
		ON condition_concept_id = concept.concept_id
	LEFT JOIN @cdm_database_schema.concept type_concept
		ON condition_type_concept_id = type_concept.concept_id
			AND condition_type_concept_id != 0
	LEFT JOIN @cdm_database_schema.concept  status_concept
		ON condition_status_concept_id = status_concept.concept_id
			AND condition_status_concept_id != 0
	LEFT JOIN #full_concept_sets full_concept_sets
		ON condition_concept_id = full_concept_sets.concept_id
	WHERE condition_concept_id != 0
	) tmp
GROUP BY generated_id,
	concept_id,
	concept_name,
	extra_data;
	
-- Visit
SELECT generated_id,
	start_day,
	end_day,
	concept_id,
	CASE WHEN concept_id = 0 THEN '' ELSE concept_name END AS concept_name,
	1 AS target
INTO #visit_context
FROM ( 
	SELECT generated_id,
		DATEDIFF(DAY, cohort_start_date, visit_start_date) AS start_day,
		DATEDIFF(DAY, cohort_start_date, visit_end_date) AS end_day,	
		visit_concept_id,
		ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY visit_start_date, visit_concept_id DESC) rn
	FROM #cohort cohort
	INNER JOIN @cdm_database_schema.visit_occurrence
		ON visit_occurrence.person_id = cohort.subject_id
			AND visit_start_date <= cohort_start_date
			AND visit_end_date >= cohort_start_date
	) visit
INNER JOIN @cdm_database_schema.concept 
	ON visit_concept_id = concept.concept_id
WHERE rn = 1;

-- Prior symptoms [-30,0)
SELECT generated_id,
	start_day,
	concept_id,
	concept_name,
	MAX(target) AS target
INTO #symptoms
FROM (
	SELECT generated_id,
		DATEDIFF(DAY, cohort_start_date, condition_era_start_date) AS start_day,
		condition_concept_id AS concept_id,
		concept_name,
		target		
	FROM #cohort cohort
	INNER JOIN @cdm_database_schema.condition_era
		ON cohort.subject_id = condition_era.person_id
			AND condition_era_start_date < cohort_start_date
			AND DATEDIFF(DAY, condition_era_start_date, cohort_start_date) <= 30
	INNER JOIN @cdm_database_schema.concept 
		ON condition_concept_id = concept_id
	INNER JOIN #full_concept_sets full_concept_sets
		ON condition_concept_id = full_concept_sets.concept_id
	WHERE concept_set_name = 'symptoms'
		AND full_concept_sets.concept_id NOT IN (SELECT concept_id FROM #full_concept_sets WHERE concept_set_name IN ('doi', 'complications'))
		
	UNION ALL 	
	
	SELECT generated_id,
		DATEDIFF(DAY, cohort_start_date, observation_date) AS start_day,
		observation_concept_id AS concept_id,
		concept_name,
		target		
	FROM #cohort cohort
	INNER JOIN @cdm_database_schema.observation
		ON cohort.subject_id = observation.person_id
			AND observation_date < cohort_start_date
			AND DATEDIFF(DAY, observation_date, cohort_start_date) <= 30
	INNER JOIN @cdm_database_schema.concept 
		ON observation_concept_id = concept_id
	INNER JOIN #full_concept_sets full_concept_sets
		ON observation_concept_id = full_concept_sets.concept_id
	WHERE concept_set_name = 'symptoms'
		AND full_concept_sets.concept_id NOT IN (SELECT concept_id FROM #full_concept_sets WHERE concept_set_name IN ('doi', 'complications'))
) tmp
GROUP BY generated_id,
	start_day,
	concept_id,
	concept_name;

-- Prior disease history
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date) AS start_day,
	condition_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #prior_disease
FROM #cohort cohort
INNER JOIN @cdm_database_schema.condition_era
	ON cohort.subject_id = condition_era.person_id
		AND condition_era_start_date < cohort_start_date
INNER JOIN @cdm_database_schema.concept 
	ON condition_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON condition_concept_id = full_concept_sets.concept_id
WHERE concept_set_name IN ('doi', 'complications')
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date),
	condition_concept_id,
	concept_name;
	
-- Post disease history
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date) AS start_day,
	condition_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #post_disease
FROM #cohort cohort
INNER JOIN @cdm_database_schema.condition_era
	ON cohort.subject_id = condition_era.person_id
		AND condition_era_start_date > cohort_start_date
INNER JOIN @cdm_database_schema.concept 
	ON condition_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON condition_concept_id = full_concept_sets.concept_id
WHERE concept_set_name IN ('doi', 'complications')
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date),
	condition_concept_id,
	concept_name;

-- Drugs prior 
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, drug_era_start_date) AS start_day,
	DATEDIFF(DAY, cohort_start_date, drug_era_end_date) AS end_day,
	drug_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #prior_drugs
FROM #cohort cohort
INNER JOIN @cdm_database_schema.drug_era
	ON cohort.subject_id = drug_era.person_id
		AND drug_era_start_date < cohort_start_date
INNER JOIN @cdm_database_schema.concept 
	ON drug_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON drug_concept_id = full_concept_sets.concept_id
WHERE concept_set_name = 'drugs'
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, drug_era_start_date),
	DATEDIFF(DAY, cohort_start_date, drug_era_end_date),
	drug_concept_id,
	concept_name;
	
-- Drugs after 
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, drug_era_start_date) AS start_day,
	DATEDIFF(DAY, cohort_start_date, drug_era_end_date) AS end_day,
	drug_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #post_drugs
FROM #cohort cohort
INNER JOIN @cdm_database_schema.drug_era
	ON cohort.subject_id = drug_era.person_id
		AND drug_era_start_date >= cohort_start_date
INNER JOIN @cdm_database_schema.concept 
	ON drug_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON drug_concept_id = full_concept_sets.concept_id
WHERE concept_set_name = 'drugs'
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, drug_era_start_date),
	DATEDIFF(DAY, cohort_start_date, drug_era_end_date),
	drug_concept_id,
	concept_name;
	
-- Treatment procedures prior 
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, procedure_date) AS start_day,
	procedure_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #prior_treatment_procedures
FROM #cohort cohort
INNER JOIN @cdm_database_schema.procedure_occurrence
	ON cohort.subject_id = procedure_occurrence.person_id
		AND procedure_date < cohort_start_date
INNER JOIN @cdm_database_schema.concept 
	ON procedure_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON procedure_concept_id = full_concept_sets.concept_id
WHERE concept_set_name = 'treatmentProcedures'
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, procedure_date),
	procedure_concept_id,
	concept_name;
	
-- Treatment procedures after 
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, procedure_date) AS start_day,
	procedure_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #post_treatment_procedures
FROM #cohort cohort
INNER JOIN @cdm_database_schema.procedure_occurrence
	ON cohort.subject_id = procedure_occurrence.person_id
		AND procedure_date >= cohort_start_date
INNER JOIN @cdm_database_schema.concept 
	ON procedure_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON procedure_concept_id = full_concept_sets.concept_id
WHERE concept_set_name = 'treatmentProcedures'
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, procedure_date),
	procedure_concept_id,
	concept_name;
	
-- alternative diagnosis within +-90 days
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date) AS start_day,
	condition_concept_id AS concept_id,
	concept_name,
	0 AS target
INTO #alternative_diagnoses
FROM #cohort cohort
INNER JOIN @cdm_database_schema.condition_era
	ON cohort.subject_id = condition_era.person_id
		AND DATEDIFF(DAY, cohort_start_date, condition_era_start_date) >= -90
		AND DATEDIFF(DAY, cohort_start_date, condition_era_start_date) <= 90
INNER JOIN @cdm_database_schema.concept 
	ON condition_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON condition_concept_id = full_concept_sets.concept_id
WHERE concept_set_name = 'alternativeDiagnosis'
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date),
	condition_concept_id,
	concept_name;
	
-- Diagnostic procedures within +-30 days 
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, procedure_date) AS start_day,
	procedure_concept_id AS concept_id,
	concept_name,
	MAX(target) AS target
INTO #diagnostic_procedures
FROM #cohort cohort
INNER JOIN @cdm_database_schema.procedure_occurrence
	ON cohort.subject_id = procedure_occurrence.person_id
		AND DATEDIFF(DAY, cohort_start_date, procedure_date) >= -30
		AND DATEDIFF(DAY, cohort_start_date, procedure_date) <= 30
INNER JOIN @cdm_database_schema.concept 
	ON procedure_concept_id = concept.concept_id
INNER JOIN #full_concept_sets full_concept_sets
	ON procedure_concept_id = full_concept_sets.concept_id
WHERE concept_set_name = 'diagnosticProcedures'
GROUP BY generated_id,
	DATEDIFF(DAY, cohort_start_date, procedure_date),
	procedure_concept_id,
	concept_name;
	
-- Measurements within +-30 days 
SELECT  generated_id,
	start_day,
	concept_id,
	concept_name,
	extra_data,
	MAX(target) AS target
INTO #measurements
FROM (
	SELECT generated_id,
		DATEDIFF(DAY, cohort_start_date, measurement_date) AS start_day,
		measurement_concept_id AS concept_id,
		concept.concept_name,
		CASE 
			WHEN value_as_concept_id != 0 AND value_concept.concept_name IS NOT NULL THEN value_concept.concept_name
			WHEN value_as_number IS NOT NULL THEN CONCAT(
				CASE WHEN operator_concept_id = 0 OR operator_concept.concept_name IS NULL THEN '' ELSE operator_concept.concept_name END,
				value_as_number,
				CASE WHEN unit_concept_id = 0 OR unit_concept.concept_name IS NULL THEN '' ELSE CONCAT(' ', unit_concept.concept_name) END,
				CASE 
					WHEN range_low IS NULL OR range_high IS NULL THEN '' 
					ELSE 
						CASE 
							WHEN value_as_number > range_high THEN ', abnormal - high'
							WHEN value_as_number < range_low THEN ', abnormal - low'
							ELSE ', normal'
						END
				END
			)
			ELSE ''
		END AS extra_data,
		target
	FROM #cohort cohort
	INNER JOIN @cdm_database_schema.measurement
		ON cohort.subject_id = measurement.person_id
			AND DATEDIFF(DAY, cohort_start_date, measurement_date) >= -30
			AND DATEDIFF(DAY, cohort_start_date, measurement_date) <= 30
	INNER JOIN @cdm_database_schema.concept 
		ON measurement_concept_id = concept.concept_id
	INNER JOIN #full_concept_sets full_concept_sets
		ON measurement_concept_id = full_concept_sets.concept_id
	LEFT JOIN @cdm_database_schema.concept value_concept
		ON value_as_concept_id = value_concept.concept_id
	LEFT JOIN @cdm_database_schema.concept unit_concept
		ON unit_concept_id = unit_concept.concept_id
	LEFT JOIN @cdm_database_schema.concept operator_concept
		ON operator_concept_id = operator_concept.concept_id
	WHERE concept_set_name = 'measurements'
) tmp
GROUP BY generated_id,
	start_day,
	concept_id,
	concept_name,
	extra_data;

-- Death
SELECT generated_id,
	DATEDIFF(DAY, cohort_start_date, death_date) AS start_day,
	cause_concept_id AS concept_id,
	CASE WHEN concept_name IS NULL THEN '' ELSE concept_name END AS concept_name,
	1 AS target
INTO #death
FROM #cohort cohort
INNER JOIN @cdm_database_schema.death
	ON cohort.subject_id = death.person_id
		AND death_date > cohort_start_date
LEFT JOIN @cdm_database_schema.concept 
	ON cause_concept_id = concept_id;
