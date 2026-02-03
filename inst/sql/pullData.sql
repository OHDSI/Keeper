-- creating concept sets 
with conceptsets as (
select concept_id as ancestor_concept_id, concept_id as descendant_concept_id, 'drugs' as category
        from @cdm_database_schema.concept where concept_id in (@drugs)
union
select concept_id, concept_id, 'doi' from @cdm_database_schema.concept where concept_id in (@doi)
union
select concept_id, concept_id, 'comorbidities' from @cdm_database_schema.concept where concept_id in (@comorbidities)
union
select concept_id, concept_id, 'symptoms' from @cdm_database_schema.concept where concept_id in (@symptoms)
union
select concept_id, concept_id, 'diagnostic_procedures' from @cdm_database_schema.concept where concept_id in (@diagnostic_procedures)
union
select concept_id, concept_id, 'measurements' from @cdm_database_schema.concept where concept_id in (@measurements)
union
select concept_id, concept_id, 'alternative_diagnosis' from @cdm_database_schema.concept where concept_id in (@alternative_diagnosis)
union
select concept_id, concept_id, 'treatment_procedures' from @cdm_database_schema.concept where concept_id in (@treatment_procedures)
union
select concept_id, concept_id, 'complications' from @cdm_database_schema.concept where concept_id in (@complications)
)
select ancestor_concept_id, 
       descendant_concept_id, 
       category
into #conceptsets
from conceptsets
;

{@use_ancestor}?{
insert into #conceptsets
select distinct ca.ancestor_concept_id, 
                ca.descendant_concept_id, 
                category
from @cdm_database_schema.concept_ancestor ca
join #conceptsets s on ca.ancestor_concept_id = s.ancestor_concept_id;
}

-- presentation: everything on day 0
with presentation as (select distinct p.person_id, c.new_id, 
                                      case
                                          when birth_datetime is not null
                                              then datediff(year, birth_datetime, cohort_start_date)
                                          else year(cohort_start_date) - year_of_birth end             as age,
                                      case when gender_concept_id = 8507 then 'Male' else 'Female' end as gender,
                                      cohort_start_date,
                                      concat(datediff(day, cohort_start_date, observation_period_start_date), ' days - ', 
                                            datediff(day, cohort_start_date, observation_period_end_date), ' days') as observation_period,
                                      case
                                          when cc2.concept_name is not null and cc3.concept_name is not null
                                              then concat(cc.concept_name, ' (', cc2.concept_name, ', ', cc3.concept_name, ');')
                                          when cc2.concept_name is not null and cc3.concept_name is null 
                                              then concat(cc.concept_name, ' (', cc2.concept_name, ');')
                                          when cc2.concept_name is null and cc3.concept_name is not null 
                                              then concat(cc.concept_name, ' (', cc3.concept_name, ');')                                          
                                          else concat(cc.concept_name, ';') end                         as concept_name,
                                       cohort_definition_id
                      from #pts_cohort c
                          join @cdm_database_schema.person p
                      on p.person_id = c.subject_id
                          left join @cdm_database_schema.observation_period op on op.person_id = p.person_id 
                                                                          and cohort_start_date > observation_period_start_date 
                                                                          and observation_period_end_date > cohort_start_date
                          left join @cdm_database_schema.condition_occurrence co on co.person_id = p.person_id and cohort_start_date = condition_start_date
                          left join @cdm_database_schema.concept cc on cc.concept_id = co.condition_concept_id and cc.concept_id!=0
                          left join @cdm_database_schema.concept cc2 on cc2.concept_id = co.condition_type_concept_id and cc2.concept_id!=0
                          left join @cdm_database_schema.concept cc3 on cc3.concept_id = co.condition_status_concept_id and cc3.concept_id!=0)
select distinct person_id, new_id, age, gender, cohort_start_date, concept_name, cohort_definition_id, observation_period
into #presentation
from presentation
order by concept_name asc
;

-- visits overlapping with the day 0
with visits as (select distinct vo.person_id,
                                datediff(day, visit_start_date, visit_end_date) as duration,
                                visit_start_date,
                                cc.concept_name                                 as visit_type,
                                cohort_start_date,
                                cohort_definition_id
                from #pts_cohort c
                    join @cdm_database_schema.visit_occurrence vo
                on vo.person_id = c.subject_id and visit_start_date <= cohort_start_date and cohort_start_date <= visit_end_date
                    join @cdm_database_schema.concept cc on cc.concept_id = vo.visit_concept_id and cc.concept_id!=0),
     visits2 as (select v.*,
                        case
                            when duration > 0 then concat(visit_type, ' (', duration, ' days)')
                            else visit_type end as                                                            visit_detail,
                        row_number() OVER (PARTITION BY person_id ORDER BY visit_start_date, visit_type desc) rn
                 from visits v)
select distinct a.person_id,
                case
                    when b.person_id is not null then concat(a.visit_detail, '->', b.visit_detail)
                    else a.visit_detail end as concept_name,
                a.cohort_start_date,    
                a.cohort_definition_id
into #visit_context
from visits2 a
    left join visits2 b
on a.person_id = b.person_id
    and b.rn = 2
where a.rn = 1;

-- comorbidities and risk factors anytime prior [-9999,0)
with conditions as (select distinct person_id,
                                    cohort_definition_id,
                                    cohort_start_date,
                                    concept_name,
                                    datediff(day, cohort_start_date, condition_era_start_date) as date_order

                    from #pts_cohort c
                        join @cdm_database_schema.condition_era co
                    on co.person_id = c.subject_id
                        and datediff(day, cohort_start_date, condition_era_start_date)<0
                        --and datediff(day, condition_era_start_date, cohort_start_date)<=365
                         join @cdm_database_schema.concept cc on cc.concept_id = condition_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and ss.category = 'comorbidities'
                         where cc.concept_id not in (select descendant_concept_id 
                                                     from #conceptsets
                                                     where category in ('symptoms', 'doi', 'complications'))
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name, 
       date_order
into #comorbidities
from conditions
order by date_order asc
;

insert into #comorbidities
select person_id, 
       cohort_definition_id, 
       cohort_start_date,
       concept_name,
       date_order
from (
    select distinct                  person_id,
                                       cohort_definition_id,
                                       cohort_start_date,
                                       concept_name,
                                       datediff(day, cohort_start_date, observation_date) as date_order
                       from #pts_cohort c
                           join @cdm_database_schema.observation co
                       on co.person_id = c.subject_id
                           and datediff(day, cohort_start_date, observation_date)<0
                         join @cdm_database_schema.concept cc on cc.concept_id = observation_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and ss.category = 'comorbidities'
                                                  where cc.concept_id not in (select descendant_concept_id 
                                                                              from #conceptsets
                                                                              where category in ('symptoms', 'doi', 'complications'))
        ) a 
order by date_order asc
;

-- prior symptoms within a prior month [-30,0)
with symptoms as (select distinct person_id,
                                       cohort_definition_id,
                                       cohort_start_date,
                                       concept_name,
                                       datediff(day, cohort_start_date, condition_era_start_date) as date_order
                       from #pts_cohort c
                           join @cdm_database_schema.condition_era co
                       on co.person_id = c.subject_id
                           and datediff(day, cohort_start_date, condition_era_start_date)<0
                           and datediff(day, condition_era_start_date, cohort_start_date)<=30
                         join @cdm_database_schema.concept cc on cc.concept_id = condition_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and ss.category = 'symptoms'
                         where cc.concept_id not in (select descendant_concept_id from #conceptsets
                                                    where category in ('doi', 'complications'))
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name, 
       date_order
into #symptoms
from symptoms
order by date_order asc
;

insert into #symptoms
select person_id, 
       cohort_definition_id, 
       cohort_start_date,
       concept_name,
       date_order
from (
    select distinct                  person_id,
                                       cohort_definition_id,
                                       cohort_start_date,
                                       concept_name,
                                       datediff(day, cohort_start_date, observation_date) as date_order
                       from #pts_cohort c
                           join @cdm_database_schema.observation co
                       on co.person_id = c.subject_id
                           and datediff(day, cohort_start_date, observation_date)<0
                           and datediff(day, observation_date, cohort_start_date)<=30
                         join @cdm_database_schema.concept cc on cc.concept_id = observation_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and ss.category = 'symptoms'
        ) a 
order by date_order asc
;

-- prior disease history [-9999,0): DOI and complications
with prior_disease as (select distinct person_id,
                                       cohort_definition_id,
                                       cohort_start_date,
                                       concept_name,
                                       datediff(day, cohort_start_date, condition_era_start_date) as date_order
                       from #pts_cohort c
                           join @cdm_database_schema.condition_era co
                       on co.person_id = c.subject_id
                           and datediff(day, cohort_start_date, condition_era_start_date)<0
                           join @cdm_database_schema.concept cc on concept_id = condition_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and ss.category in  ('complications','doi')
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #prior_disease
from prior_disease
order by date_order asc
;

-- disease history after (0,9999]: DOI and complications
with after_disease as (select distinct person_id,
                                       cohort_definition_id,
                                       cohort_start_date,
                                       concept_name,
                                       datediff(day, cohort_start_date, condition_era_start_date) as date_order
                       from #pts_cohort c
                           join @cdm_database_schema.condition_era co
                       on co.person_id = c.subject_id
                           and datediff(day, condition_era_start_date, cohort_start_date)<0
                         join @cdm_database_schema.concept cc on concept_id = condition_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and ss.category in ('complications','doi')
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #after_disease
from after_disease
order by date_order asc
;

-- drugs all time prior [-9999, 0), relies on drug_era
with drugs as (select distinct person_id,
                               cohort_definition_id,
                               cohort_start_date,
                               concat(concept_name, ' (day ', datediff(day, cohort_start_date, drug_era_start_date),
                                      ', for ',
                                      datediff(day, drug_era_start_date, drug_era_end_date), ' days);') as concept_name,
                               datediff(day, cohort_start_date, drug_era_start_date)                    as date_order

               from #pts_cohort c
                   join @cdm_database_schema.drug_era co
               on co.person_id = c.subject_id
                   and datediff(day, cohort_start_date, drug_era_start_date)<0
                   --and datediff(day, drug_era_start_date, cohort_start_date)<=365
                   join @cdm_database_schema.concept cc on cc.concept_id = drug_concept_id and cc.concept_id!=0
                   join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'drugs'  
               )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #prior_drugs
from drugs
order by date_order asc
;


-- drugs all time after [0;9999], relies on drug_era
with drugs as (select distinct person_id,
                               cohort_definition_id,
                               cohort_start_date,
                               concat(concept_name, ' (day ', datediff(day, cohort_start_date, drug_era_start_date),
                                      ', for ',
                                      datediff(day, drug_era_start_date, drug_era_end_date), ' days);') as concept_name,
                               datediff(day, cohort_start_date, drug_era_start_date)                    as date_order

               from #pts_cohort c
                   join @cdm_database_schema.drug_era co
               on co.person_id = c.subject_id
                   and datediff(day, cohort_start_date, drug_era_start_date)>=0
                   --and datediff(day, drug_era_start_date, cohort_start_date)<=365
                   join @cdm_database_schema.concept cc on cc.concept_id = drug_concept_id and cc.concept_id!=0
                   join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'drugs'  
               )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #after_drugs
from drugs
order by date_order asc
;

-- treatment procedures prior [-9999,0)
with treatment as (select distinct person_id,
                                   cohort_definition_id,
                                   cohort_start_date,
                                   concept_name,
                                   datediff(day, cohort_start_date, procedure_date) as date_order
                   from #pts_cohort c
                       join @cdm_database_schema.procedure_occurrence po
                   on po.person_id = subject_id
                       and datediff(day, cohort_start_date, procedure_date)<0
                         join @cdm_database_schema.concept cc on cc.concept_id = procedure_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'treatment_procedures'
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #prior_treatment_procedures
from treatment
order by date_order asc
;
 
-- treatment procedures [0,9999]
with treatment as (select distinct person_id,
                                   cohort_definition_id,
                                   cohort_start_date,
                                   concept_name,
                                   datediff(day, cohort_start_date, procedure_date) as date_order
                   from #pts_cohort c
                       join @cdm_database_schema.procedure_occurrence po
                   on po.person_id = subject_id
                       and datediff(day, procedure_date, cohort_start_date)<=0
                         join @cdm_database_schema.concept cc on cc.concept_id = procedure_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'treatment_procedures'
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #after_treatment_procedures
from treatment
order by date_order asc
;

-- alternative diagnosis within +-90 days [-90, 90].
with dx as (select distinct person_id,
                            cohort_definition_id,
                            cohort_start_date,
                            concept_name,
                            datediff(day, cohort_start_date, condition_era_start_date) as date_order
            from #pts_cohort c
                join @cdm_database_schema.condition_era co
            on co.person_id = c.subject_id
                and datediff(day, cohort_start_date, condition_era_start_date)<=90
                and datediff(day, condition_era_start_date, cohort_start_date)<=90
                         join @cdm_database_schema.concept cc on cc.concept_id = condition_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'alternative_diagnosis'
                         where cc.concept_id not in (select descendant_concept_id from #conceptsets where category='doi')
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #alternative_diagnosis
from dx
order by date_order asc
;

-- diagnostic procedures around the day 0 [-30;+30]
with diagnostics as (select distinct person_id,
                                     cohort_definition_id,
                                     cohort_start_date,
                                     concept_name,
                                     datediff(day, cohort_start_date, procedure_date) as date_order
                     from #pts_cohort c
                         left join @cdm_database_schema.procedure_occurrence po
                     on po.person_id = subject_id
                         and datediff(day, cohort_start_date, procedure_date)<=30
                         and datediff(day, procedure_date, cohort_start_date)<=30
                         join @cdm_database_schema.concept cc on cc.concept_id = procedure_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'diagnostic_procedures'
                           )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #diagnostic_procedures
from diagnostics
order by date_order asc
;

-- measurements around day 0 [-30;+30].
with meas as (
-- value_as_number
    select person_id,
           cohort_definition_id, 
           cohort_start_date,
           case when range_high is not null and range_low is not null then
                concat(cc.concept_name, ' (', value_as_number, ' ', cc2.concept_name, ', ',
                        case when value_as_number > range_high then 'abnormal - high'
                             when value_as_number < range_low then 'abnormal - low'
                             else 'normal' end, ', day ', datediff(day, cohort_start_date, measurement_date), ');')
                else concat(cc.concept_name, ' (', value_as_number, ' ', cc2.concept_name, ', day ', 
                            datediff(day, cohort_start_date, measurement_date), ');') end as concept_name, 
           datediff(day, cohort_start_date, measurement_date) as date_order 
    from #pts_cohort c
        join @cdm_database_schema.measurement m
    on m.person_id = subject_id
        and datediff(day, cohort_start_date, measurement_date)<=30
        and datediff(day, measurement_date, cohort_start_date)<=30
                         join @cdm_database_schema.concept cc on cc.concept_id = measurement_concept_id
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'measurements'    
        left join @cdm_database_schema.concept cc2 on cc2.concept_id = unit_concept_id and cc2.concept_id!=0
        where value_as_number is not null

    union

-- value_as_concept_id
    select person_id,
           cohort_definition_id, 
           cohort_start_date,
           concat(cc.concept_name, ' (', cc2.concept_name, ', day ', datediff(day, cohort_start_date, measurement_date), ');') as concept_name,
           datediff(day, cohort_start_date, measurement_date) as date_order
    from #pts_cohort c
        join @cdm_database_schema.measurement m
    on m.person_id = subject_id
        and datediff(day, cohort_start_date, measurement_date)<=30
        and datediff(day, measurement_date, cohort_start_date)<=30
                         join @cdm_database_schema.concept cc on cc.concept_id = measurement_concept_id
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'measurements'   
        join @cdm_database_schema.concept cc2 on cc2.concept_id = value_as_concept_id
        and value_as_concept_id is not null and value_as_concept_id!=0

    union

    -- everything else
    select person_id, 
           cohort_definition_id, 
           cohort_start_date,
           concat(cc.concept_name, ' (', 'day ', datediff(day, cohort_start_date, measurement_date), ');') as concept_name, 
           datediff(day, cohort_start_date, measurement_date) as date_order
    from #pts_cohort c
        join @cdm_database_schema.measurement m
    on m.person_id = subject_id
        and datediff(day, cohort_start_date, measurement_date)<=30
        and datediff(day, measurement_date, cohort_start_date)<=30
                         join @cdm_database_schema.concept cc on cc.concept_id = measurement_concept_id and cc.concept_id!=0
                         join #conceptsets ss on ss.descendant_concept_id = cc.concept_id and category = 'measurements'   
        and value_as_number is null and (value_as_concept_id is null or value_as_concept_id=0)
        )
select person_id, 
       cohort_definition_id, 
       cohort_start_date, 
       concept_name,
       date_order
into #measurements
from meas c
order by date_order asc;

--death  
with death as (select distinct person_id,
                                       cohort_definition_id,
                                       cohort_start_date,
                                       concat(concept_name, ' (day ',
                                              datediff(day, cohort_start_date, death_date),
                                              ');')                                               as concept_name
                       from #pts_cohort c
                           join @cdm_database_schema.death d
                       on d.person_id = c.subject_id and death_date>=cohort_start_date
                           join @cdm_database_schema.concept cc on cc.concept_id = cause_concept_id and cc.concept_id!=0
)
select person_id, 
       cohort_definition_id, 
       cohort_start_date,
       concept_name
into #death
from death
order by concept_name asc
;
