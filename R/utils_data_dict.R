#' These set the data dictionaries to avoid pulls and populate fields
#' 
#' 
#' @noRd


#Simple dictionary used for read-in from REDCap
redcap_dict <- list(
  adrc_key = "adc_sub_id",
  redcap_key = "record_id",
  subj_event = "subject_data_arm_1",
  visit_event = "visit_info_arm_1",
  event_col = "redcap_event_name",
  visit_col = "redcap_repeat_instance"
)



#Dictionary for filtering minimum datasets
#min_fields is a list that defines each of the tables that gets pulled
#Primary used is by drop_invalid_rows which can check either ALL columns or ANY columns
min_fields <- list(
  registry = c("contact_status"),
  referral = c("refer_dt"),
  nacc = c("a1_form_dt"),
  neuroimage = c("abeta_visit_dt", "tau_visit_dt"),
  clinical_cohort = c("contact_dt", "refer_dt", "screen_dt", "adc_clin_core_dt"),
  nacc_visit = c("a1_form_dt", "d1_form_dt"),
  neuro = c("c2_form_dt")
)
#Other uses are for filtering datasets for study involvement e.g. SS-DIAD check




#Dictionary for slicing the ADRC dataframe and filtering minimum datasets
#min_fields is a list that defines each of the tables that gets pulled
#If that field isn't filled out, it's not considered a valid entry
#e.g. the registry requires contact status; the UDS data requires an A1 date (same as the NACC requirement)

explorer_slice <- list(
  visit_type = c("Baseline", "Most Recent", "Longitudinal"),
  df_action = list(
    quote(df_slicer(df, "first", drop_min = "nacc_visit")),
    quote(df_slicer(df, "last", drop_min = "nacc_visit")),
    quote(df_slicer(df, "longitudinal", drop_min = "nacc_visit"))
  )
)
slice_dict <- list(
  slice_type = c("first", "last", "all", "first NACC", "longitudinal"),
  completion_slice = list(type = c("Most Recent Visit", "All Visits"),
                          slice = c("last", "all")),
  explorer_slice = explorer_slice
)


#Available studies for visualization and building enrollment fields
study_choices <- list(
    var = c("NACC Participant", "adc_bval", "ADRC Participant"),
    annot = c("nacc", "bval", "adrc"),
    name = c("NACC ADRC Cohort", "BVal", "Full UAB ADRC Cohort"),
    start_dt = c("2024-04-01", "2020-01-01", "2018-01-01"),
    date_field = c("adc_clin_core_dt", "adc_bval_dt", "adrc_enroll_dt"),
    visit_date_field = c("a1_form_dt", "a1_form_dt", "a1_form_dt")
)

#Interactions to add
interact_dict <- list(
  var1 = c("regist_race"),
  var2 = c("regist_sex"),
  name = c("race_and_sex")
)

#Data dictionaries - factors and continuous

data_dict <- list(
  data_col = c("race", "sex", "ccc_numeric_stg", "ccc_syndrm_stg", "race_and_sex", "refer_type", "regist_race", "regist_sex", "cdrglob"),
  new_name = c("NACC Race", "NACC Sex", "AD Numeric Stage", "AD Syndromal Stage", "Race and Sex", "Referral Type", "Race", "Sex", "Global CDR"),
  col_levels = list(
    c(1, 2, 3, 4, 5, 50, 99),
    c(1, 2),
    c(1, 2, 3, 4, 5, 6),
    c(1, 2, 3),
    c("1_2", "2_2", "3_2", "4_2", "5_2", "6_2", "1_1", "2_1", "3_1", "4_1", "5_1", "6_1"),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10),
    c(5, 3, 1, 4, 2, 6),
    c(1, 2, 3),
    c(0, 0.5, 1, 2, 3)
  ),
  col_labels = list(
    c("White", "Black / AA", "Am Indian / Alaska Native", "Pacific Islander", "Asian", "Other", "Unknown"),
    c("Male", "Female"),
    c("1 - Cognitively Normal", "2 - Transitional", "3 - Mild Cognitive Impairment",
      "4 - Mild Dementia", "5 - Moderate Dementia", "6 - Severe Dementia"),
    c("Cognitively Unimpaired", "Mild Cognitive Impairment", "Dementia"),
    c("Am Indian Male", "Asian Male", "Black Male", "Pacific Islander Male", "White Male", "Other Male",
      "Am Indian Female", "Asian Female", "Black Female", "Pacific Islander Female", "White Female", "Other Female"),
    c("Clinic", "Alzheimer's Association", "Community Outreach", "Outside Studies", "Lectures", "Clinical Trials.gov",
      "Family of Affected", "Flyer", "Word of Mouth", "REGARDS", "Other"),
    c("White", "Black / AA", "Am Indian / Alaska Native", "Pacific Islander", "Asian", "Other"),
    c("Female", "Male", "Other"),
    c("0.0 - None", "0.5 - MCI", "1.0 - Mild", "2.0 - Moderate", "3.0+ - Severe")
  ),
  drop_levels = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
)


#Used for coercing certain variables to numeric continuous
#This isn't being actively used at the moment
data_dict_cont <- list(
  data_col = c("cs_age"),
  new_name = c("Age")
)


#Variables that are cast from factors to continuous
#Main use is within the nacc_curr dataset to coerce certain factors into better numeric representations
data_dict_fact_to_cont <- list(
  data_col = c("educ", "cdrglob"),
  new_name = c("Education", "Global CDR"),
  cast_to_na = list(c("99"), c(NULL)),
  levels_exclude = list(c(NULL), c(NULL))
)




#Referral and eligibility dictionaries
#1/15/2023 - Status and Registry dictionary has been added

status_dict <- list(
  levels = c(1:6, 9, -888),
  labels = c("Unable to Contact", "Unable to Contact Clinic Referral", "Informational Contact Only", "Research Registrant",
             "Insufficient Info", "Withdrawl from Registry", "Initial Creation", "-888"),
  na_fill = 5,
  enroll_flow_drop = c("Unable to Contact", "Insufficient Info"),
  .bad = "-888"
)

regist_dict <- list(
  levels = c(1,2,0,3,4),
  labels = c("Interested in Research", "Undecided", "Not Interested", "Cannot Contact/Withdrawl", "Unknown"),
  na_fill = 4,
  retained = c("Interested in Research", "Cannot Contact/Withdrawl")
)

refer_dict <- list(
  levels = c(1, 2, 3, 4),
  labels = c("Eligible & Willing", "Screen Failure", "Declined", "Unknown / Under Review"),
  na_fill = 4,
  #This portion is used for filtering out specific studies in the enrollment plot
  sub_study_decline = c("ADRC Cohort", "BVal"),
  decline_fill = list(c(3), c(3)),
  decline_replace = c(1, 1)
)

elig_dict <- list(
  map_from = c(4, 5, 6),
  map_to = c(1),
  levels = c(1, 2, 7, 3),
  labels = c("Enrolled", "Scheduled for Consent", "Unable to Complete", "Never Enrolled"),
  na_fill = 3,
  sub_study_decline = c("ADRC Cohort", "BVal"),
  decline_fill = list(c(3), c(3)),
  decline_replace = c(1, 1)
)

enroll_dict <- list(
  levels_to_drop = c(2, 3, 7),
  levels = c(1, 4, 5, 6),
  labels = c("Currently Following", "Follow-up Loss", "Asked to be Removed", "Death"),
  death_level = 6,
  death_field = "deceased",
  loss_level = 5,
  enroll_level = 1
)

#Dictionary to distinguish between enrolled and decline participants, recasts groups and filters on certain columns / levels
decline_dict <- list(
  old_group = c("Race", "Sex"),
  new_group = c("Prescreen Race", "Prescreen Sex"),
  subset_col = c("refer"),
  subset_lvls = c("Declined", "Unknown / Under Review")
)


#Dictionary for enrollment flow 

enroll_flow_dict <- list(
  var = c("regist", "refer", "elig", "enroll"), 
  title = c("Research Status of Registered Individuals", "Registrants with Research Interest Ever", "Eligible & Willing", "Enrolled Participants"),
  study_filter = c(FALSE, FALSE, FALSE, TRUE)
)
#Note the study_filter option is used by enrollment flow but the FALSE, FALSE, TRUE design causes issues with synthetic data
#This is because of how the synthetic data is generated compared to the otherwise nesting design used by things like BVal
#This may become an issue as more studies are rolled in but I'm not entirely sure


#The referral dictionary, used to do relabels of main referral types and subtypes
#Also directs collapsing into a vector and the NACC ADRC recast of prior participants before P20
#Finally, does the table labelling used in the prior version

refer_details_dict <- list(
  type = list(
    field = "refer_type", 
    plot_field = "Main Referral Source",
    labels = c("Clinician Referral", "Community Outreach", "Study Referral", "Self-Referral", "Prior ADC Subject", "Unknown Source"),
    plot_labels = c("Clinician\nReferral", "Community\nEvent", "Study\nReferral", "Self\nReferral", "Prior ADC\nSubject", "Unknown\nSource"),
    recast = "Prior ADC Subject"
  ),
  sub_type = list(
    recast_col = c("refer_clinic", "refer_outreach", "refer_study", "refer_self"),
    coalesce = c("refer_clinic", "refer_outreach", "refer_study", "refer_self"),
    plot_source_map = c(
      clinic = "Clinician Referral", 
      outreach = "Community Outreach", 
      study = "Study Referral", 
      self = "Self-Referral"
    )
  ),
  gsub_string = "refer_",
  clinic = list(
    field = "refer_clinic",
    plot_field = "Clinical Referrals",
    labels = c(
      "Memory Disorders",
      "Primary Care or Geriatrics",
      "Other Clinic or Unknown"
    ),
    plot_labels = c(
      "Memory\nDisorders",
      "Primary Care\nor Geriatrics",
      "Other Clinic\nor Unknown"
    )
  ),
  outreach = list(
    field = "refer_outreach",
    plot_field = "Community Outreach",
    labels = c("Outreach Event", "ADRC Lecture", "Titusville Event"),
    plot_labels = c("Outreach\nEvent", "ADRC\nLecture", "Titusville\nEvent")
  ),
  study = list(
    field = "refer_study",
    plot_field = "Study Referrals",
    labels = c(
      "Outside Study",
      "REGARDS Referral",
      "Pickering Referral",
      "Other Referral"
    ),
    plot_labels = c(
      "Outside\nStudy",
      "REGARDS\nReferral",
      "Pickering\nReferral",
      "Other\nReferral"
    )
  ),
  self = list(
    field = "refer_self",
    plot_field = "Self-Directed",
    labels = c(
      "Alzheimer Association",
      "Clinical Trials Site",
      "Family of Affected",
      "Advertisement",
      "Word of Mouth",
      "Other Source"
    ),
    plot_labels = c(
      "Alzheimer's\nAssociation",
      "Clinical\nTrials Site",
      "Family of\nAffected",
      "Adertisement\nor Article",
      "Word of\nMouth",
      "Other\nSource"
    )
  ),
  
  #Used specifically for building by_year tables; see create_refer_details function for quote() context
  table_cast = list(
    enroll = list(
      data_col = c(
        "refer_dt",
        quote(group_curr),
        "coalesced_enroll",
        "Referral Source",
        "coalesced_details"
      ),
      new_name = c(
        "Referral Date",
        quote(group_curr),
        "Status",
        "Source",
        "Details"
      )
    ),
    decline = list(
      data_col = c("refer_dt", "Race", "Referral Source", "screen_decline_reasn"),
      new_name = c("Referral Date", "Race", "Source", "Decline Reason")
    )
  )
)


#Data dictionary for specialized column labels / table headers - includes hard returns

refer_labels_dict <- list(
  Full = list(
    var_curr = c("Referral Source"),
    df_levels = list(
      c(
        "Clinic",
        "Alzheimer's Association",
        "Community Event",
        "Outside Studies",
        "Lectures",
        "Clinical Trials.gov",
        "Family of Affected",
        "Flyer",
        "Word of Mouth",
        "Other"
      )
    ),
    plot_labels = list(
      c(
        "Clinic",
        "Alzheimer's\nAssociation",
        "Community\nEvent",
        "Outside\nStudies",
        "Lectures",
        "Clinical\nTrials.gov",
        "Family of\nAffected",
        "Flyer",
        "Word of\nMouth",
        "Other"
      )
    ),
    declined = c(FALSE, TRUE),
    colorset = c("Set3", "Set1")
  )
)


#The old referral dictionary

# refer_details_dict_old <- list(enroll = list(
#   data_col = c("prescreen_dt", "Race", "Referral Source", "prescreen_refer_othr_spec"),
#   new_name = c("Contact Date", "Race", "Source", "Details")),
#   decline = list(
#     data_col = c("prescreen_dt", "Race", "Referral Source", "prescreen_decline_reasn"),
#     new_name = c("Contact Date", "Race", "Source", "Decline Reason")))





#Collection component dictionary


data_dict_component <- list(
  data_col = c("adc_imag", "cons_csf", "cons_brain"),
  new_name = c("Neuroimaging", "CSF / Lumbar", "Brain Donation Interest"),
  
  #Remapping
  comp_annot = c("PET", "CSF", "BD"),
  radio_to_bin = c(TRUE, FALSE, FALSE),
  #We use this for non-binary radio buttons (would be 1 otherwise for check boxes)
  
  #Because of complexities in the flattening, it's better to provide a set of options for what we map from to what we map to
  #This is a very similar convention to what's in elig_dict although we find it easiest to go entry by entry
  #As default, if we flatten but there isn't an entry in flatten_radio, we can default to the usual binarization behavior
  recast_radio = list(
    PET = list(
      map_from = c(2, NA),
      map_to = c(0, 0)
    )
  ),
  #In the case of merely mapping the presence of data entry, we can use "any" to map to 1 and NA (i.e. missing) to 0
  
  #Covariate choices, these may need to be collapsed e.g. a Race is in the registry but not actually enrolled
  covar_list = c("Race", "Sex", "AD Syndromal Stage"),
  covar_collapse = c(TRUE, TRUE, FALSE),
  
  #We use all_table_rows with plotting but all_table_cols with the tables
  all_tables_rows = c("Total"),
  all_tables_cols = c("V1", "Total"),
  
  #Building out names is now dependent on what component we're checking
  #We have the order based on the numeric levels
  #This is followed by the labels used to build the raw factor levels
  #Then the labels used to build the table column names
  #An important point about all these is they are done AFTER any flatten calls to reduc factors levels using flatten_radio
  build_order = list(
    PET = c(1, 0, 2),
    CSF = c(1, 2, 0),
    BD = c(1, 2, 0)
  ),
  build_names = list(
    PET = c("Enrolled", "Unlisted",  "Screen Fail"),
    CSF = c("Yes", "Undecided", "No"),
    BD = c("Yes", "Undecided", "No")
  ),
  build_table_cols = list(
    PET = c("Enrolled", "Unlisted", "Screen Fail"),
    CSF = c("Yes", "Undecided", "No"),
    BD = c("Yes", "Undecided", "No")
  ),
  
  #Enrollment filtering
  enroll_field = "enroll",
  follow_only = c("Currently Following"),
  
  #Grep strings for plotting and annotations
  
  #These are the rows that don't get plotted
  #Like grep_string for table annotation you need to be able to catch something from each entry in build_names
  row_filter = "^No",
  #row_filter = "^(No|Screen Fail)" #This would be another option that would also drop the Screen Fail row from PET
  
  #Grep string used during the final table name annotation process
  #This essentially needs to cover all cases in build_table_cols specifically
  grep_string = "Yes |Enrolled "
)



data_dict_completion <- list(
  visit = list(
    label_col = c("coord", "nurse", "rater", "clinical", "cc_conf"),
    data_col = c("clin_coord_comp", "clin_nurse_comp", "memtest_comp", "clin_exam_comp","ccc_syndrm_stg"),
    plot_name = c("Coordinator Eval", "Nurse Eval", "Neuropsych", "Clinical Exam", "Consensus Dx"),
    radio_to_check = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    reduc = c(NA, NA, NA, NA, "flat"),
    reduc_fields = c(NA, NA, NA, NA, NA)
  ),
  collect = list(
    label_col = c("plasma", "csf_col", "pib_pet", "tau_pet"),
    data_col = c("biospec_comp", "csf_comp", "neuroimag_pib_comp", "neuroimag_tau_comp"),
    plot_name = c("Plasma", "CSF Draw", "PiB PET", "Tau PET"),
    radio_to_check = c(TRUE, TRUE, TRUE, TRUE),
    reduc = c(NA, "reduc", "reduc", "reduc"),
    reduc_fields = c(NA, "cons_csf", "adc_imag", "adc_imag")
  ),
  shipped = list(
    label_col = c("ncrad_biom", "ncrad_gwas", "hudson_alpha"),
    data_col = c("ncrad_blood_ship_dt", "ncrad_gwas_ship_dt", "hudson_ship_dt"),
    plot_name = c("NCRAD Plasma", "NCRAD GWAS", "Hudson Alpha"),
    radio_to_check = c(FALSE, FALSE, FALSE),
    reduc = c("flat", "flat", "flat"),
    
    #reduc_fields is a list for the shipped portion to also filter out participants who couldn't complete a component
    # e.g. exclude those who are enrolled for CSF but couldn't get a draw
    reduc_fields = list(c("biospec_comp"), c("biospec_comp"), c("biospec_comp")),
    reduc_vals = list(c(1), c(1), c(1))
  )
)




data_dict_inventory <- list(
    dropped_fields = c(
      "record_id",
      "redcap_survey_identifier",
      "adni_guid",
      "ppmp_guid",
      "adc_cntr_id",
      "subj_dob",
      "subj_age",
      "brain_id",
      "form_ver_num",
      "enroll_status",
      "subject_info_complete",
      "biospec_complete"
    ),
    
    bulk_drop_string = c("_dt$"),
    
    data_col = c(
      "buffy_count",
      "csfl_count",
      "dna_count",
      "pax_count",
      "pbmc_count",
      "plasma_count",
      "serum_count",
      "urine_count"
    ),
    
    biospec_names = c(
      "Buffy Coat",
      "CSF",
      "DNA",
      "PAXgene",
      "PBMC",
      "Plasma",
      "Serum",
      "Urine"
    )
  )
                            
