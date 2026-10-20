SELECT concept_set_name,
  SUM(person_count) AS person_count
FROM (
  SELECT concept_set_name,
    COUNT(DISTINCT person_id) AS person_count
  FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON condition_concept_id = descendant_concept_id
	INNER JOIN #concept_sets concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('doi', 'hypernym')
	GROUP BY concept_set_name
	  
	UNION ALL
	 
	SELECT concept_set_name,
	  COUNT(DISTINCT person_id) AS person_count
  FROM @cdm_database_schema.observation
	INNER JOIN @cdm_database_schema.concept_ancestor
	  ON observation_concept_id = descendant_concept_id
	INNER JOIN #concept_sets concept_sets
	  ON ancestor_concept_id = concept_id
	WHERE concept_set_name IN ('doi', 'hypernym')
	GROUP BY concept_set_name
) tmp
GROUP BY concept_set_name;
