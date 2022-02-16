## Census Data (ACS 1 yr) ----
# Define years
years <- c(2011:2019)


acs_data_raw <- map_dfr(
    years,
    ~ get_acs(
        geography = 'state',
        variables = c(
            # total_state_pop = 'B02001_001',
            'Hispanic or Latino' = 'B03002_012',
            # Total Hispanic or Latino
            'White' = 'B03002_003',
            # White (Not Hispanic or Latino)
            'Black or African American' = 'B03002_004',
            # Black or African American (Not Hispanic or Latino)
            'Native American or Alaska Native' = 'B03002_005',
            # American Indian and Alaska Native (Not Hispanic or Latino)
            'Asian' = 'B03002_006',
            # Asian (Not Hispanic or Latino)
            'Pacific Islander' = 'B03002_007',
            # Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)
            'Other' = 'B03002_008',
            # Some other race (Not Hispanic or Latino)
            'Multiple' = 'B03002_009'# Two or more races (Not Hispanic or Latino)
        ),
        summary_var = c(total_state_pop = 'B02001_001'),
        survey = 'acs1',
        # use 'acs1' or 'acs5' to get 1 or 5 year acs
        state = 'CA',
        geometry = FALSE,
        # set to TRUE to get as geospatial data
        year = .x
    ),
    .id = "year"
)

setwd("05_shiny_app/data")
# write.csv(acs_data_raw, file = "acs_data_raw.csv")

# Pull Dicennial census data for 2020
# dcensus_data_raw <- get_decennial(
#         geography = 'state',
#         variables = c(
#             # total_state_pop = 'B02001_001',
#             'Hispanic or Latino' = 'B03002_012',
#             # Total Hispanic or Latino
#             'White' = 'B03002_003',
#             # White (Not Hispanic or Latino)
#             'Black or African American' = 'B03002_004',
#             # Black or African American (Not Hispanic or Latino)
#             'Native American or Alaska Native' = 'B03002_005',
#             # American Indian and Alaska Native (Not Hispanic or Latino)
#             'Asian' = 'B03002_006',
#             # Asian (Not Hispanic or Latino)
#             'Pacific Islander' = 'B03002_007',
#             # Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)
#             'Other' = 'B03002_008',
#             # Some other race (Not Hispanic or Latino)
#             'Multiple' = 'B03002_009'# Two or more races (Not Hispanic or Latino)
#         ),
#         summary_var = c(total_state_pop = 'B02001_001'),
#         state = 'CA',
#         geometry = FALSE,
#         # set to TRUE to get as geospatial data
#         year = 2020
#     )
