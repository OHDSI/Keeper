DROP TABLE IF EXISTS #concept_ratios;

WITH doi_persons AS (
	SELECT DISTINCT person_id
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (
	  SELECT concept_id
	  FROM #concept_sets
	  WHERE concept_set_name = 'doi'
	)
)
SELECT concept_id,
	concept_name,
	vocabulary_id,
	concept_set_name,
	concept_person_count,
	concept_doi_person_count,
	doi_person_count,
	person_count,
	concept_doi_person_count / CAST(concept_person_count AS FLOAT) AS ppv
INTO #concept_ratios
FROM (
	SELECT concept_id,
		concept_name,
		vocabulary_id,
		concept_set_name,
		SUM(concept_person_count) AS concept_person_count,
		SUM(concept_doi_person_count) AS concept_doi_person_count
	FROM (
		SELECT concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name,
		  COUNT(*) AS concept_person_count,
		  SUM(CASE WHEN doi_persons.person_id IS NOT NULL THEN 1 ELSE 0 END) AS concept_doi_person_count
		FROM (
			SELECT DISTINCT person_id,
				concept_id,
				concept_name,
				vocabulary_id,
				concept_set_name
			FROM @cdm_database_schema.drug_exposure
			INNER JOIN @cdm_database_schema.concept_ancestor
			  ON drug_concept_id = descendant_concept_id
			INNER JOIN #concept_sets
			  ON ancestor_concept_id = concept_id
			WHERE concept_set_name IN ('drugs')
		) tmp
		LEFT JOIN doi_persons
		  ON tmp.person_id = doi_persons.person_id
		GROUP BY concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name

		UNION ALL
		
		SELECT concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name,
		  COUNT(*) AS concept_person_count,
		  SUM(CASE WHEN doi_persons.person_id IS NOT NULL THEN 1 ELSE 0 END) AS concept_doi_person_count
		FROM (
			SELECT DISTINCT person_id,
				concept_id,
				concept_name,
				vocabulary_id,
				concept_set_name
			FROM @cdm_database_schema.condition_occurrence
			INNER JOIN @cdm_database_schema.concept_ancestor
			  ON condition_concept_id = descendant_concept_id
			INNER JOIN #concept_sets
			  ON ancestor_concept_id = concept_id
			WHERE concept_set_name IN ('symptoms', 'complications')
		) tmp
		LEFT JOIN doi_persons
		  ON tmp.person_id = doi_persons.person_id
		GROUP BY concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name
		  
		UNION ALL
		
		SELECT concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name,
		  COUNT(*) AS concept_person_count,
		  SUM(CASE WHEN doi_persons.person_id IS NOT NULL THEN 1 ELSE 0 END) AS concept_doi_person_count
		FROM (
			SELECT DISTINCT person_id,
				concept_id,
				concept_name,
				vocabulary_id,
				concept_set_name
			FROM @cdm_database_schema.observation
			INNER JOIN @cdm_database_schema.concept_ancestor
			  ON observation_concept_id = descendant_concept_id
			INNER JOIN #concept_sets
			  ON ancestor_concept_id = concept_id
			WHERE concept_set_name IN ('symptoms')
		) tmp
		LEFT JOIN doi_persons
		  ON tmp.person_id = doi_persons.person_id
		GROUP BY concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name
		
		UNION ALL
		
		SELECT concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name,
		  COUNT(*) AS concept_person_count,
		  SUM(CASE WHEN doi_persons.person_id IS NOT NULL THEN 1 ELSE 0 END) AS concept_doi_person_count
		FROM (
			SELECT DISTINCT person_id,
				concept_id,
				concept_name,
				vocabulary_id,
				concept_set_name
			FROM @cdm_database_schema.procedure_occurrence
			INNER JOIN @cdm_database_schema.concept_ancestor
			  ON procedure_concept_id = descendant_concept_id
			INNER JOIN #concept_sets
			  ON ancestor_concept_id = concept_id
			WHERE concept_set_name IN ('diagnosticProcedures', 'treatmentProcedures')
		) tmp
		LEFT JOIN doi_persons
		  ON tmp.person_id = doi_persons.person_id
		GROUP BY concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name
		  
		UNION ALL
		
		SELECT concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name,
		  COUNT(*) AS concept_person_count,
		  SUM(CASE WHEN doi_persons.person_id IS NOT NULL THEN 1 ELSE 0 END) AS concept_doi_person_count
		FROM (
			SELECT DISTINCT person_id,
				concept_id,
				concept_name,
				vocabulary_id,
				concept_set_name
			FROM @cdm_database_schema.measurement
			INNER JOIN @cdm_database_schema.concept_ancestor
			  ON measurement_concept_id = descendant_concept_id
			INNER JOIN #concept_sets
			  ON ancestor_concept_id = concept_id
			WHERE concept_set_name IN ('measurements')
		) tmp
		LEFT JOIN doi_persons
		  ON tmp.person_id = doi_persons.person_id
		GROUP BY concept_id,
		  concept_name,
		  vocabulary_id,
		  concept_set_name

	) concept_counts
	GROUP BY concept_id,
		concept_name,
		vocabulary_id,
		concept_set_name
) unique_concept_counts
CROSS JOIN (
	SELECT COUNT(*) AS doi_person_count
	FROM doi_persons
) doi_person_count
CROSS JOIN (
	SELECT COUNT(*) AS person_count
	FROM @cdm_database_schema.person
) person_count;
