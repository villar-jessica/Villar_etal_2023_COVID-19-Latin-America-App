df <- read_excel('input/Dataset Jessica v21.xlsx') %>% 
  mutate(total_deaths_per_100khabitants = ConfirmedDeaths * 100000 / population,
         total_cases_per_100khabitants = ConfirmedCases * 100000 / population,
         case_fatality_rate = ConfirmedDeaths / ConfirmedCases,
         new_deaths_per_100khabitants = new_deaths * 100000 / population,
         new_deaths_per_100khabitants_mm7 = new_deaths_mm7 * 100000 / population,
         new_cases_per_100khabitants = new_cases * 100000 / population,
         new_cases_per_100khabitants_mm7 = new_cases_mm7 * 100000 / population,
         people_at_least_first_dose_vaccinated_per_population = people_vaccinated / population,
         people_at_least_two_doses_vaccinated_per_population = people_fully_vaccinated / population,
         total_vaccinations_per_100khabitants = total_vaccinations * 100000 / population,
         total_boosters_per_100khabitants = total_boosters * 100000 / population,
         new_vaccinations_per_100khabitants = new_vaccinations * 100000 / population,
         total_tests_per_100khabitants = total_tests * 100000 / population) %>% 
  select(-c('CountryCode','RegionName','RegionCode','Jurisdiction', 
            contains('Flag'),
            'StringencyIndexForDisplay', 'StringencyLegacyIndex', 'StringencyLegacyIndexForDisplay', 
            'GovernmentResponseIndexForDisplay', 'ContainmentHealthIndexForDisplay', 'EconomicSupportIndexForDisplay',
            'Day', 'Year',
            'country_region',	'sub_region_1',	'metro_area',
            'M1_Wildcard', 'PIB','le','eys','gnipc',
            'hospital_beds_per_thousand', 'new_vaccinations_smoothed', 'total_vaccinations_per_hundred', 'people_vaccinated_per_hundred', 'total_boosters_per_hundred', 'new_vaccinations_smoothed_per_million', 'new_people_vaccinated_smoothed_per_hundred',
            'total_vaccinations','total_boosters', 'new_vaccinations', 'total_tests', 'ConfirmedCases', 'ConfirmedDeaths',
            'new_cases','new_cases_mm7','new_deaths','new_deaths_mm7','new_tests','people_vaccinated','people_fully_vaccinated'
  )
  ) %>% 
  rename('Total deaths per 100,000 inhabitants' = 'total_deaths_per_100khabitants',
         'Total cases per 100,000 inhabitants' = 'total_cases_per_100khabitants',
         'Case fatality rate' = 'case_fatality_rate',
         'New deaths per 100,000 inhabitants' = 'new_deaths_per_100khabitants',
         '7-day moving average of new deaths per 100,000 inhabitants' = 'new_deaths_per_100khabitants_mm7',
         'New cases per 100,000 inhabitants' = 'new_cases_per_100khabitants',
         '7-day moving average of new cases per 100,000 inhabitants' = 'new_cases_per_100khabitants_mm7',
         'Percentage of the population with at least one vaccine dose' = 'people_at_least_first_dose_vaccinated_per_population',
         'Percentage of the population with at least two vaccine doses' = 'people_at_least_two_doses_vaccinated_per_population',
         'Total number of COVID-19 vaccination doses administered per 100,000 inhabitants' = 'total_vaccinations_per_100khabitants',
         'Total number of COVID-19 vaccination booster doses administered per 100,000 inhabitants' = 'total_boosters_per_100khabitants',
         'New COVID-19 vaccination doses administered per 100,000 inhabitants' = 'new_vaccinations_per_100khabitants',
         'Total tests for COVID-19 per 100,000 inhabitants' = 'total_tests_per_100khabitants',
         'Median age of the population' = 'median_age',
         'Share of the population that is 65 years and older' = 'aged_65_older',
         'Share of the population that is 70 years and older' = 'aged_70_older',
         'Population' = 'population',
         'GPD per capita' = 'gdp_per_capita',
         'Share of the population living in extreme poverty' = 'extreme_poverty',
         'Mobility trends for places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters compared to a baseline' = 'retail_and_recreation_percent_change_from_baseline',
         'Mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies compared to a baseline' = 'grocery_and_pharmacy_percent_change_from_baseline',
         'Mobility trends for places like national parks, public beaches, marinas, dog parks, plazas, and public gardens compared to a baseline' = 'parks_percent_change_from_baseline',
         'Mobility trends for places like public transport hubs such as subway, bus, and train stations compared to a baseline' = 'transit_stations_percent_change_from_baseline',
         'Mobility trends for places of work compared to a baseline' = 'workplaces_percent_change_from_baseline',
         'Mobility trends for places of residence compared to a baseline' = 'residential_percent_change_from_baseline',
         'Record closings of schools and universities' = 'C1_School closing',
         'Record closings of workplaces' = 'C2_Workplace closing',
         'Record cancelling public events' = 'C3_Cancel public events',
         'Record limits on gatherings' = 'C4_Restrictions on gatherings',
         'Record closing of public transport' = 'C5_Close public transport',
         'Record orders to "shelter-in-place" and otherwise confine to the home' = 'C6_Stay at home requirements',
         'Record restrictions on internal movement between cities/regions' = 'C7_Restrictions on internal movement',
         'Record restrictions on international travel for foreign travellers' = 'C8_International travel controls',
         'Record if the government is providing direct cash payments to people who lose their jobs or cannot work' = 'E1_Income support',
         'Record if the government is freezing financial obligations for households' = 'E2_Debt/contract relief',
         'Announced economic stimulus spending' = 'E3_Fiscal measures',
         'Announced offers of Covid-19 related aid spending to other countries' = 'E4_International support',
         'Record presence of public info campaigns' = 'H1_Public information campaigns',
         'Record government policy on who has access to testing' = 'H2_Testing policy',
         'Record government policy on contact tracing after a positive diagnosis' = 'H3_Contact tracing',
         'Announced short term spending on healthcare system, eg hospitals, masks, etc' = 'H4_Emergency investment in healthcare',
         'Announced public spending on Covid-19 vaccine development' = 'H5_Investment in vaccines',
         'Record policies on the use of facial coverings outside the home' = 'H6_Facial Coverings',
         'Record policies for vaccine delivery for different groups' = 'H7_Vaccination policy',
         'Record policies for protecting elderly people' = 'H8_Protection of elderly people',
         'Reports the existence of a prioritised plan for vaccine rollout' = 'V1_Vaccine Prioritisation (summary)',
         'Reports whether any categories of people are receiving vaccines' = 'V2A_Vaccine Availability (summary)',
         'Reports lowest age range of general population being vaccinated' = 'V2B_Vaccine age eligibility/availability age floor (general population summary)',
         'Reports lowest age range of at risk population being vaccinated' = 'V2C_Vaccine age eligibility/availability age floor (at risk summary)',
         'Reports the number of categories selected from: clinically vulnerable/chronic illness/significant underlying health condition (excluding elderly and disabled), disabled people, pregnant people and people living with a vulnerable/shielding person or other priority group' = 'V2D_Medically/ clinically vulnerable (Non-elderly)',
         'Reports the number of categories selected from: educators, primary and secondary school students and tertiary education students' = 'V2E_Education',
         'Reports the number of categories selected from: police/first responders, airport/border/airline staff, factory workers, frontline retail workers, military, other high contact professions/groups (taxi drivers, security guards) and frontline/essential workers (when subcategories not specified)' = 'V2F_Frontline workers  (non healthcare)',
         'Reports the number of categories selected from: staff working in an elderly care home and healthcare workers/carers (excluding care home staff)' = 'V2G_Frontline workers  (healthcare)',
         'Reports the overall approach taken to vaccine funding, whether paid by the individual or the government' = 'V3_Vaccine Financial Support (summary)',
         'Reports the existence of a requirement to be vaccinated' = 'V4_Mandatory Vaccination (summary)',
         'Stringency index' = 'StringencyIndex',
         'Government response index' = 'GovernmentResponseIndex',
         'Containment health index' = 'ContainmentHealthIndex',
         'Economic support index' = 'EconomicSupportIndex',
         'HDI' = 'hdi',
         'Average number of years of education received by people ages 25 and older' = 'mys',
         'Gini index' = 'Gini',
         'Percentage of people ages 20-79 who have type 1 or type 2 diabetes' = 'Diabetes_Prevalence_2021',
         'Share of informal employment in total employment on agricultural activites' = 'pct_informal_ag_jobs',
         'Share of informal employment in total employment on non agricultural activites' = 'pct_informal_n_ag_jobs',
         'Share of informal employment in total employment on agricultural and non agricultural activites' = 'pct_informal_tot_jobs',
         'Hospitals per 100,000 inhabitants' = 'Hospital_Density_Per_100k')

write.csv2(df, file = 'input/latin_america_covid.csv')
