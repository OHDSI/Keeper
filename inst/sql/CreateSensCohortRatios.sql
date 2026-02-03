DROP TABLE IF EXISTS #concept_ratios;

SELECT concept_id,
  concept_name,
  vocabulary_id,
  concept_set_name,
  concept_person_count,
  doi_person_count,
  CAST(concept_person_count AS FLOAT) / doi_person_count AS ratio
INTO #concept_ratios
FROM (
	SELECT concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name,
	  COUNT(DISTINCT(person_id)) AS concept_person_count
	FROM @cdm_database_schema.drug_exposure
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON drug_concept_id = descendant_concept_id
	INNER JOIN #concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('drugs')
	GROUP BY concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name

	UNION ALL

	SELECT concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name,
	  COUNT(DISTINCT(person_id)) AS concept_person_count
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms', 'complications')
	GROUP BY concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name
	  
	UNION ALL

	SELECT concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name,
	  COUNT(DISTINCT(person_id)) AS concept_person_count
	FROM @cdm_database_schema.observation
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON observation_concept_id = descendant_concept_id
	INNER JOIN #concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('symptoms')
	GROUP BY concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name
	  
	UNION ALL

	SELECT concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name,
	  COUNT(DISTINCT(person_id)) AS concept_person_count
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON procedure_concept_id = descendant_concept_id
	INNER JOIN #concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('diagnosticProcedures', 'treatmentProcedures')
	GROUP BY concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name
	  
	UNION ALL

	SELECT concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name,
	  COUNT(DISTINCT(person_id)) AS concept_person_count
	FROM @cdm_database_schema.measurement
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON measurement_concept_id = descendant_concept_id
	INNER JOIN #concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('measurements')
	GROUP BY concept_id,
	  concept_name,
	  vocabulary_id,
	  concept_set_name
) concept_counts
CROSS JOIN (
	SELECT COUNT(DISTINCT(person_id)) AS doi_person_count
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (
	  SELECT concept_id
	  FROM #concept_sets
	  WHERE concept_set_name = 'doi'
	)
) doi_person_count;
