rm(list=ls())

# --- load functions from github ---

devtools::source_url("https://github.com/stefaniemeliss/ambition_theme/blob/main/ambition_theme.R?raw=TRUE")

# --- read in data ---

dir = "C:/Users/stefanie.meliss/OneDrive - Ambition Institute/research_projects/Ark Feedback Orientation and Sims/Data/Final data files for analysis/FO"

setwd(dir)

df <- read.csv("All_data_FO.csv")

# --- prepare data ---

# filter and select
df_fo <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO, FO.2) 
# reshape wide -> long
dfl_fo <- reshape2::melt(df_fo)
# format 
dfl_fo$variable <- ifelse(dfl_fo$variable == "FO", "1", "2")
dfl_fo$variable <- factor(dfl_fo$variable, levels = c("1", "2"))
dfl_fo$construct <- "Feedback orientation"
dfl_fo$comp <- "Summed score"

# filter and select
df_fow <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_wtd, FO2_wtd) 
# reshape wide -> long
dfl_fow <- reshape2::melt(df_fow)
# format 
dfl_fow$variable <- ifelse(dfl_fow$variable == "FO_wtd", "1", "2")
dfl_fow$variable <- factor(dfl_fow$variable, levels = c("1", "2"))
dfl_fow$construct <- "Feedback orientation"
dfl_fow$comp <- "Weighted score"

# filter and select
df_fo_e <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_efficacy, FO_efficacy.2) 
# reshape wide -> long
dfl_fo_e <- reshape2::melt(df_fo_e)
# format 
dfl_fo_e$variable <- ifelse(dfl_fo_e$variable == "FO_efficacy", "1", "2")
dfl_fo_e$variable <- factor(dfl_fo_e$variable, levels = c("1", "2"))
dfl_fo_e$construct <- "FO self-efficacy"
dfl_fo_e$comp <- "Summed score"

# filter and select
df_fo_ew <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_efficacy_wtd, FO2_efficacy_wtd) 
# reshape wide -> long
dfl_fo_ew <- reshape2::melt(df_fo_ew)
# format 
dfl_fo_ew$variable <- ifelse(dfl_fo_ew$variable == "FO_efficacy_wtd", "1", "2")
dfl_fo_ew$variable <- factor(dfl_fo_ew$variable, levels = c("1", "2"))
dfl_fo_ew$construct <- "FO self-efficacy"
dfl_fo_ew$comp <- "Weighted score"



## rbind all


# --- create plot in facets: NOT USED ---

dfl <- rbind(dfl_fo, dfl_fow)
dfl <- rbind(dfl, dfl_fo_e)
dfl <- rbind(dfl, dfl_fo_ew)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO self-efficacy"))

plt <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point", size = 3, color = navy) +
  ambition_theme + xlab("Survey") + ylab("Self-report measure") +
  facet_grid(construct ~ comp, scales = "free_y")
plt

# --- create facets for FO only ---

dfl <- rbind(dfl_fo, dfl_fow)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO self-efficacy"))

plt_fo <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point", size = 3, color = navy) +
  ambition_theme + xlab("") + ylab("FO") +
  facet_grid(. ~ comp) #+ coord_cartesian(ylim = c(64, 88))
plt_fo

# --- create facets for FO efficacy only ---

dfl <- rbind(dfl_fo_e, dfl_fo_ew)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO self-efficacy"))

plt_fo_e <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point", size = 3, color = navy) +
  ambition_theme + xlab("Survey") + ylab("FO self-efficacy") +
  facet_grid(. ~ comp) #+ coord_cartesian(ylim = c(16, 22))
plt_fo_e


##

ggpubr::ggarrange(
  plt_fo, plt_fo_e, nrow = 2)

