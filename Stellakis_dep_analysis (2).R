library(dplyr)
library(readxl)
library(meta)
library(dmetar)

rm(list = ls())

# Define data
data_dir <- "/Users/oriellastellakis/Desktop/UCL MSc/modules/dissertation/data_analysis/analysis"
output_dir <- "/Users/oriellastellakis/Desktop/UCL MSc/modules /dissertation /data_analysis /analysis"

filename <- file.choose()
main_df <- read_excel(filename, sheet = "Sheet1")

# main_df <- read_excel(paste(data_dir, "Oriella_data extraction_2024_08_29_final.xlsx", sep = ""), sheet = "Sheet1")

dep_df <- main_df

# Change key variables to numeric
dep_df$TBI_N <- as.numeric(dep_df$TBI_N)
dep_df$Depression_N <- as.numeric(dep_df$Depression_N)

# Filter out rows with missing data in the following variables...

# TBI_N
dep_df <- dep_df %>%
  filter(!is.na(TBI_N))

# Depression_N
dep_df <- dep_df %>%
  filter(!is.na(Depression_N))

# Run meta-analysis
dep_meta <- metaprop(Depression_N, TBI_N,
                      studlab = Study,
                      sm = "PFT",
                      method.tau = "DL",
                      method.cs = "WS",
                      data = dep_df)

# Forest plot and save to pdf file
pdf(file = paste(output_dir, "dep_forestplot.pdf", sep = ""), width = 10, height = 36)
forest(dep_meta)
dev.off()

#
# Subgroup analyses
#

# Study design
dep_des_df <- dep_df %>%
  filter(StudyDesign != "randomized controlled trial")

dep_design_meta <- metaprop(Depression_N, TBI_N,
                             studlab = Study,
                             subgroup = StudyDesign,
                             sm = "PFT",
                             method.tau = "DL",
                             method.cs = "WS",
                             data = dep_des_df)

pdf(file = paste(output_dir, "dep_subgroup_design_forestplot.pdf", sep = ""), width = 10, height = 40)
forest(dep_design_meta)
dev.off()

# TBI Severity
dep_sev_df <- dep_df %>%
  filter(!is.na(TBI_Severity))

dep_severity_meta <- metaprop(Depression_N, TBI_N,
                               studlab = Study,
                               subgroup = TBI_Severity,
                               sm = "PFT",
                               method.tau = "DL",
                               method.cs = "WS",
                               data = dep_sev_df)

pdf(file = paste(output_dir, "dep_subgroup_severity_forestplot.pdf", sep = ""), width = 10, height = 34)
forest(dep_severity_meta)
dev.off()

# Depression measure type
dep_measure_meta <- metaprop(Depression_N, TBI_N,
                              studlab = Study,
                              subgroup = Depression_ID_Method,
                              sm = "PFT",
                              method.tau = "DL",
                              method.cs = "WS",
                              data = dep_df)

pdf(file = paste(output_dir, "dep_subgroup_measure_forestplot.pdf", sep = ""), width = 10, height = 38)
forest(dep_measure_meta)
dev.off()

# Setting
dep_sett_meta <- metaprop(Depression_N, TBI_N,
                           studlab = Study,
                           subgroup = Setting,
                           sm = "PFT",
                           method.tau = "DL",
                           method.cs = "WS",
                           data = dep_df)

pdf(file = paste(output_dir, "dep_subgroup_setting_forestplot.pdf", sep = ""), width = 10, height = 40)
forest(dep_sett_meta)
dev.off()

# Study population
dep_pop_meta <- metaprop(Depression_N, TBI_N,
                          studlab = Study,
                          subgroup = Population,
                          sm = "PFT",
                          method.tau = "DL",
                          method.cs = "WS",
                          data = dep_df)

pdf(file = paste(output_dir, "dep_subgroup_population_forestplot.pdf", sep = ""), width = 10, height = 40)
forest(dep_pop_meta)
dev.off()

# Continent
dep_cont_meta <- metaprop(Depression_N, TBI_N,
                           studlab = Study,
                           subgroup = Continent,
                           sm = "PFT",
                           method.tau = "DL",
                           method.cs = "WS",
                           data = dep_df)

pdf(file = paste(output_dir, "dep_subgroup_continent_forestplot.pdf", sep = ""), width = 10, height = 40)
forest(dep_cont_meta)
dev.off()

#
# Meta-regressions
#

# Age
metareg(dep_meta, ~ MeanAge)

# Gender balance
metareg(dep_meta, ~ PercentFemales)

# Year study published
metareg(dep_meta, ~ Publishedyear)

# Percent mTBI
metareg(dep_meta, ~ Percent_mTBI)

# Percent mod/severe TBI
metareg(dep_meta, ~ Percent_modsevTBI)

#
# Publication bias assessment
#

# Funnel plot
pdf(file = paste(output_dir, "dep_funnelplot.pdf", sep = ""))
funnel(dep_meta)
dev.off()

# Egger's test for funnel plot asymmetry
metabias(dep_meta, method.bias="linreg")

#
# Outlier and influence analysis
#

# Outliers
dmetar::find.outliers(dep_meta)

# Influence diagnostics (identifies Dismuke-Greer 2020 as influential study)
dep_meta_inf_diagnostics <- dmetar::InfluenceAnalysis(dep_meta, random = TRUE)
dep_meta_inf_diagnostics

pdf(file = paste(output_dir, "dep_influenceplot.pdf", sep = ""))
plot(dep_meta_inf_diagnostics, "influence")
dev.off()

# Leave1Out sensitivity analysis
metainf(dep_meta, pooled = "random")

