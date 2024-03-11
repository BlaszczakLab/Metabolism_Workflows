#### Sample R Script
#### Creation Date: December 14, 2022
#### Contributors: Heili Lowman

# This is a sample script to begin the record in the "R_scripts" folder.

# All scripts in the Metabolism workflow should be well annotated, and
# follow the same general workflow. In addition, all file paths must
# use universal structuring (e.g., using the `here` package) so that code
# is able to be used across multiple peoples' computers.

# Load packages.
library(tidyverse)
library(lubridate)
library(here)

# Load data.
df <- read_csv("data_raw/nwis_11462500_input.csv")

# Perform necessary operations.

df <- df %>%
  mutate(Date = ymd_hms(solar.time))

(fig1 <- ggplot(df, aes(x = Date, y = DO.obs)) +
  geom_line(color = "aquamarine") +
  theme_bw())


print(df)
# End of script.
