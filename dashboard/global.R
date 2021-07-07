library(here)

# load data if not already in memory
if(!exists("hobo") || !exists("feed_drink_df")){
    load(here("data/full_10_month_analysis_result_summary_only_dashboard.Rda"))
    
    hobo <- dashboard_full_analysis[["HOBO"]]
    insentec <- dashboard_full_analysis[["Insentec"]]
    
    rm(dashboard_full_analysis)
}
