SELECT SUM(person_count) AS person_count
FROM (
  SELECT COUNT(DISTINCT person_id) AS person_count
  FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (
	  SELECT concept_id
	  FROM #concept_sets
	  WHERE concept_set_name = 'doi'
	)
	  
	 UNION ALL
	 
	SELECT COUNT(DISTINCT person_id) AS person_count
  FROM @cdm_database_schema.observation
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON observation_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (
	  SELECT concept_id
	  FROM #concept_sets
	  WHERE concept_set_name = 'doi'
	)
) tmp;
