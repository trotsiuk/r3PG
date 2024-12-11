
# Required packages:
requiredPackages <- c("r3PG","tidyverse","ggplot2","data.table","purrr","multidplyr","stringr","readxl","dplyr","tidyr")

# install/load required packages:
if (exists("requiredPackages")) {
  # install required packages that are not installed yet:
  new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  # load required packages:
  lapply(requiredPackages, library, character.only=T)
}

rm(new.packages,requiredPackages)




Path1 <- "C:/Forrester/Models/3-PG/carrying_capacity/Rdata/data/"



f_loc <- paste(Path1,"Unevenaged_example.xlsx", sep="")


out_3PG <- run_3PG(site        = read_xlsx(f_loc, 'site'),
                     species     = read_xlsx(f_loc, 'species'),
                     climate     = prepare_climate(climate = read_xlsx(f_loc, 'climate'),
                                                   from = read_xlsx(f_loc, sheet = 'site')$from,
                                                   to = read_xlsx(f_loc, sheet = 'site')$to),
                     thinning    = read_xlsx(f_loc, 'thinning'),
                     parameters  = read_xlsx(f_loc, 'parameters'),
                     size_dist   = read_xlsx(f_loc, 'sizeDist'),
                     settings = list(light_model = 2,      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                     transp_model = 2,     # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                     phys_model = 2,       # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                     height_model = 1,     # '1' - linear (default); '2' - non-linear
                                     correct_bias = 0,       # '0' - no (default); '1' - yes
                                     calculate_d13c = 0,   # '0' - no (default); '1' - yes
                                     mort_model = 2),      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                     check_input = TRUE, df_out = TRUE)




# plot the development of dbh and tree density for each cohort

sel_var <- c('stems_n', 'dbh') # sort(unique(out_3PG$variable))

out_3PG %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_classic()


# examine the whole stand basal area (sum of all cohorts)
x <- out_3PG %>% dplyr::filter(variable %in% "basal_area") %>% dplyr::group_by(date) %>% dplyr::summarise(total_basal_area = sum(na.omit(value))) %>%
  dplyr::mutate(year = as.numeric(format(date, "%Y")),
                julian_day = as.numeric(format(date, "%j")),
                year_dec = year + julian_day/366) %>% 
  dplyr::filter(year_dec > 2090 & year_dec < 2300)
par(mfrow=c(1,1),  oma=c(0.7,0.4,0,0))
maxVariable <- 100
minYear <- 2090
maxYear <- 2300
par(las = 0, mar = c(2.5, 3, 0.1, 0.5))
plot(NULL, axes = FALSE, ylim=c(0,maxVariable), xlim=c(minYear, maxYear), xlab="",  type = "n", ylab="")
axis(side=2, cex.axis=0.9, tcl=-0.2, las=1, labels = NA, tick=TRUE, at=seq(from=0, to=maxVariable, by=round(maxVariable/4,0)))
axis(side=2, cex.axis=0.9, lwd = 0, line = -0.6, las=1, at=seq(from=0, to=maxVariable, by=round(maxVariable/4,0)))
axis(side=1, cex.axis=0.9, tcl=-0.2, las=1, labels = NA, tick=TRUE, at=c(1900, 1950, 2000, 2050, 2100, 2150, 2200, 2250, 2300, 2350, 2400, 2450))
axis(side=1, cex.axis=0.9, lwd = 0, line = -0.6, las=1, at=c(1900, 1950, 2000, 2050, 2100, 2150, 2200, 2250, 2300, 2350, 2400, 2450))
box()
lines(x$year_dec, x$total_basal_area, col="lightsalmon3", lty='solid')
mtext(c(expression(paste('Basal area (',~m^2,~ha^-1,')',sep=''))), side=2, line=1.7, cex=0.9)
mtext(c(expression(paste('Year',sep=''))), side=1, line=1.5, cex=0.9)


