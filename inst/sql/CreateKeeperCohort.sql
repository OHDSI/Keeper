SELECT subject_id,
	generated_id,
	cohort_start_date
INTO #cohort
FROM (
	SELECT subject_id,
		ROW_NUMBER() OVER (ORDER BY NEWID()) AS generated_id,
		cohort_start_date
	FROM @cohort_table
	WHERE cohort_definition_id = @cohort_definition_id
{@use_person_ids} ? {		AND subject_id IN (SELECT person_id FROM #person_ids) }
)
WHERE generated_id <= @sample_size;


