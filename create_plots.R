rm(list=ls())

# --- load functions from github ---

devtools::source_url("https://github.com/stefaniemeliss/ambition_theme/blob/main/ambition_theme.R?raw=TRUE")
# library(ggplot2)
# ambition_theme <- theme_bw()
# coral <- "pink"
# black40 <- "lightgray"

library(dplyr)

# --- read in data ---

cd <- getwd()

dir = "C:/Users/stefanie.meliss/OneDrive - Ambition Institute/research_projects/Ark Feedback Orientation and Sims/Data/Final data files for analysis/FO"

df <- read.csv(file.path(dir, "All_data_FO.csv"))

# --- prepare data ---

# filter and select: Feedback orientation
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

# filter and select: Feedback orientation weighted
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

# filter and select: Feedback efficacy
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

# filter and select: Feedback efficacy weighted
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

# filter and select: Feedback utility 
df_fo_u <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_utility, FO_utility.2) 
# reshape wide -> long
dfl_fo_u <- reshape2::melt(df_fo_u)
# format 
dfl_fo_u$variable <- ifelse(dfl_fo_u$variable == "FO_utility", "1", "2")
dfl_fo_u$variable <- factor(dfl_fo_u$variable, levels = c("1", "2"))
dfl_fo_u$construct <- "FO utility"
dfl_fo_u$comp <- "Summed score"

# filter and select: Feedback utility weighted
df_fo_uw <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_utility_wtd, FO2_utility_wtd) 
# reshape wide -> long
dfl_fo_uw <- reshape2::melt(df_fo_uw)
# format 
dfl_fo_uw$variable <- ifelse(dfl_fo_uw$variable == "FO_utility_wtd", "1", "2")
dfl_fo_uw$variable <- factor(dfl_fo_uw$variable, levels = c("1", "2"))
dfl_fo_uw$construct <- "FO utility"
dfl_fo_uw$comp <- "Weighted score"

# filter and select: Feedback social awareness 
df_fo_s <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_awareness, FO_awareness.2) 
# reshape wide -> long
dfl_fo_s <- reshape2::melt(df_fo_s)
# format 
dfl_fo_s$variable <- ifelse(dfl_fo_s$variable == "FO_awareness", "1", "2")
dfl_fo_s$variable <- factor(dfl_fo_s$variable, levels = c("1", "2"))
dfl_fo_s$construct <- "FO social awareness"
dfl_fo_s$comp <- "Summed score"

# filter and select: Feedback social awareness weighted
df_fo_sw <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_aware_wtd, FO2_aware_wtd) 
# reshape wide -> long
dfl_fo_sw <- reshape2::melt(df_fo_sw)
# format 
dfl_fo_sw$variable <- ifelse(dfl_fo_sw$variable == "FO_aware_wtd", "1", "2")
dfl_fo_sw$variable <- factor(dfl_fo_sw$variable, levels = c("1", "2"))
dfl_fo_sw$construct <- "FO social awareness"
dfl_fo_sw$comp <- "Weighted score"

# filter and select: Feedback accountability 
df_fo_a <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_accountability, FO_accountability.2) 
# reshape wide -> long
dfl_fo_a <- reshape2::melt(df_fo_a)
# format 
dfl_fo_a$variable <- ifelse(dfl_fo_a$variable == "FO_accountability", "1", "2")
dfl_fo_a$variable <- factor(dfl_fo_a$variable, levels = c("1", "2"))
dfl_fo_a$construct <- "FO accountability"
dfl_fo_a$comp <- "Summed score"

# filter and select: Feedback accountability weighted
df_fo_aw <- df %>%
  filter(Exclusion_H2==0) %>%
  select(participant, FO_account_wtd, FO2_account_wtd) 
# reshape wide -> long
dfl_fo_aw <- reshape2::melt(df_fo_aw)
# format 
dfl_fo_aw$variable <- ifelse(dfl_fo_aw$variable == "FO_account_wtd", "1", "2")
dfl_fo_aw$variable <- factor(dfl_fo_aw$variable, levels = c("1", "2"))
dfl_fo_aw$construct <- "FO accountability"
dfl_fo_aw$comp <- "Weighted score"



## rbind all


# # --- create plot in facets: NOT USED ---
# 
# dfl <- rbind(dfl_fo, dfl_fow)
# dfl <- rbind(dfl, dfl_fo_e)
# dfl <- rbind(dfl, dfl_fo_ew)
# dfl$construct <- factor(dfl$construct, levels = c("FO", "FO self-efficacy"))
# 
# plt <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
#   stat_summary(fun = "mean", geom = "line") +
#   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
#   stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
#   ambition_theme + xlab("Survey") + ylab("Self-report measure") +
#   facet_grid(construct ~ comp, scales = "free_y")
# plt

# --- set some overall parameter --- #

size_dot = 2
ymin = 17
ymax = 29

# --- create facets for FO only ---

dfl <- rbind(dfl_fo, dfl_fow)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO self-efficacy"))

plt_fo <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("") + ylab("FO") +
  facet_grid(. ~ comp) + coord_cartesian(ylim = c(70, 90))
plt_fo

plt_fo_spag <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  geom_point(size = .5, color = black40) +
  geom_line(aes(group = participant), color = black40) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("") + ylab("FO") +
  facet_grid(. ~ comp) # + coord_cartesian(ylim = c(70, 90))
plt_fo_spag

# plt_fo <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
#   geom_point(size = .5, color = black40, position = position_jitter(.1)) +
#   geom_line(aes(group = participant), color = black40, position = position_jitter(.1)) +
#   stat_summary(fun = "mean", geom = "line") +
#   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
#   stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
#   ambition_theme + xlab("") + ylab("FO") +
#   facet_grid(. ~ comp) # + coord_cartesian(ylim = c(70, 90))
# plt_fo

# --- create facets for FO efficacy only ---

dfl <- rbind(dfl_fo_e, dfl_fo_ew)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO self-efficacy"))

plt_fo_e <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("Survey") + ylab("FO self-efficacy") +
  facet_grid(. ~ comp) + coord_cartesian(ylim = c(ymin, ymax))
plt_fo_e

plt_fo_e_spag <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  geom_point(size = .5, color = black40) +
  geom_line(aes(group = participant), color = black40) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("Survey") + ylab("FO self-efficacy") +
  facet_grid(. ~ comp) #+ coord_cartesian(ylim = c(ymin, ymax))
plt_fo_e_spag


## --- Combine to create Figure 2 ---

ggpubr::ggarrange(
  plt_fo, plt_fo_e, nrow = 2, align = "hv")
ggsave(filename = file.path(cd, "Figure2.jpg"), 
       units = "cm", width = 16, height = 12)


# --- create facets for FO accountability only ---

dfl <- rbind(dfl_fo_a, dfl_fo_aw)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO accountability"))

plt_fo_a <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("") + ylab("FO accountability") +
  facet_grid(. ~ comp) + coord_cartesian(ylim = c(ymin, ymax))
plt_fo_a

plt_fo_a_spag <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  geom_point(size = .5, color = black40) +
  geom_line(aes(group = participant), color = black40) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("") + ylab("FO accountability") +
  facet_grid(. ~ comp) #+ coord_cartesian(ylim = c(ymin, ymax))
plt_fo_a_spag

# --- create facets for FO social awareness only ---

dfl <- rbind(dfl_fo_s, dfl_fo_sw)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO social awareness"))

plt_fo_s <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("") + ylab("FO social awareness") +
  facet_grid(. ~ comp) + coord_cartesian(ylim = c(ymin, ymax))
plt_fo_s

plt_fo_s_spag <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  geom_point(size = .5, color = black40) +
  geom_line(aes(group = participant), color = black40) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("") + ylab("FO social awareness") +
  facet_grid(. ~ comp) #+ coord_cartesian(ylim = c(ymin, ymax))
plt_fo_s_spag


# --- create facets for FO utility only ---

dfl <- rbind(dfl_fo_u, dfl_fo_uw)
dfl$construct <- factor(dfl$construct, levels = c("FO", "FO utility"))

plt_fo_u <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("Survey") + ylab("FO utility") +
  facet_grid(. ~ comp) + coord_cartesian(ylim = c(ymin, ymax))
plt_fo_u

plt_fo_u_spag <- ggplot(data = dfl, aes(x = variable, y = value, group = 1)) +
  geom_point(size = .5, color = black40) +
  geom_line(aes(group = participant), color = black40) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("Survey") + ylab("FO utility") +
  facet_grid(. ~ comp) #+ coord_cartesian(ylim = c(ymin, ymax))
plt_fo_u_spag


## --- Combine to create Figure S1 ---

ggpubr::ggarrange(
  plt_fo_a, plt_fo_s, plt_fo_u, nrow = 3, align = "hv")
ggsave(filename = file.path(cd, "FigureS1.jpg"), 
       units = "cm", width = 16, height = 18)
ggpubr::ggarrange(
  plt_fo_a_spag, plt_fo_s_spag, plt_fo_u_spag, nrow = 3, align = "hv")

# --- create figures for summary for educators ---

plt_fo <- ggplot(data = dfl_fow, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("Survey") + ylab("FO") +
  coord_cartesian(ylim = c(70, 90))
plt_fo
ggsave(filename = file.path(cd, "summaryforeds1.jpg"), 
       units = "cm", width = 16, height = 6)

plt_fo_e <- ggplot(data = dfl_fo_ew, aes(x = variable, y = value, group = 1)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1) +
  stat_summary(fun = "mean", geom = "point",size = size_dot, color = coral) +
  ambition_theme + xlab("Survey") + ylab("FO self-efficacy") +
  coord_cartesian(ylim = c(ymin, ymax))
plt_fo_e
ggsave(filename = file.path(cd, "summaryforeds2.jpg"), 
       units = "cm", width = 16, height = 6)
