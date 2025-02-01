# Elles et al. (2024). Supporting conservation planning in a national 
# biodiversity hotspot -Projecting species composition across a groundwater level
# gradient using a demographic forest model

# This Script incorporates the PPA Model arranged to be parameterized by the 
# combined demographic rates  the Inital state from 2020. 
# This script to run this script use the the R scripts 
# "04_run_tidy_combined_rates_elevated_groundwater.R" located in the folder 
# "model_scripts".

main_cohorts_tidy = function(
    mainfolder,       # main folder in which files for this simulation are stored
    spdata_path,      # path to the file with vital rates and allometry
    initdata_path    # path to the file with initial data, or NA if simulation is to start from bare ground
    ) {
  # define basic settings: 
  deltaT = 5         # model timestep
  maxT = 100        # maximum simulation time
  dnot = 5         # dbh with which recruits are "born" in cm
  PA = 10000        # simulation area in m² 
  cutT = 1           # timesteps, at which stats shall be recorded
  mincohortN = 0.01  # minimum number of trees in a cohort, before cohort gets removed
  
  spdata = read_table(paste(mainfolder, spdata_path, sep="/"), show_col_types = F) %>% 
    mutate(sp = as.integer(factor(sp)))
  
  if(is.na(initdata_path)) {
    initdata = NA
  } else {
    initdata = as_tibble(read.table(paste(mainfolder, initdata_path, sep="/"), header = F))
  }
  
  # prepare initial data: 
  data = prepare.initdata(initdata, spdata, dnot, deltaT, mincohortN, PA)
  
  
  # record data to save: 
  all_cohorts_out = data
  stats_out = calc.stats(data,time=0)
  dbh_dist = derive.dbh.distribution(data, time=0)
  
  # MAIN LOOP: .............................................................................
  for(time.tmp in seq(deltaT,maxT,by=deltaT)) {
    print(time.tmp)
    
    # Step 1: Mortality
    data = data %>% 
      mutate(n = ifelse(cl == 1, 
                        n * (1 - spdata$mu1[sp]) ^ deltaT, 
                        n * (1 - spdata$mu2[sp]) ^ deltaT)) %>% 
      filter(n > mincohortN)
    
    # Step 2: Growth
    
    data = data %>% 
      mutate(dbh = ifelse(cl == 1, 
                          dbh + spdata$G1[sp] * deltaT, 
                          dbh + spdata$G2[sp] * deltaT))
    
    # Step 3: Recruitment
    data = data %>% 
      bind_rows(recruitment(spdata, dnot, deltaT, PA, time.tmp))
    
    # Step 4: Assign canopy layer
    data = assign.layers(data, spdata, PA) %>% 
      filter(cl < 3,         # kill everything higher than layer 3
             n > mincohortN) # filter for mincohortN again
    
    # Step 5: Record stats
    all_cohorts_out = bind_rows(all_cohorts_out, data %>% mutate(time = time.tmp))
    if(time.tmp %% cutT == 0) {
      stats_out = bind_rows(stats_out, calc.stats(data, time.tmp))
      dbh_dist = bind_rows(dbh_dist, derive.dbh.distribution(data, time.tmp))
    }
    
  } #end main loop...........................................................................

  #safe output
  write_csv(all_cohorts_out, paste0(mainfolder, "/all_cohorts_out.csv"))
  write_csv(stats_out, paste0(mainfolder, "/stats_out.csv"))
  write_csv(dbh_dist, paste0(mainfolder, "/dbh_dist_out.csv"))
  
  plot_temp = plot.output(all_cohorts_out, mainfolder)
  ggsave(paste0(mainfolder, "/plot_1.jpg"), plot_temp, width = 10, height = 8)
  
  return(list(all_cohorts_out, stats_out, dbh_dist))
}


prepare.initdata = function(initdata, spdata, dnot, deltaT, mincohortN, PA) {
  # create initial data (if NA) or get it in shape: 
  if(is_tibble(initdata)) {
    data = initdata %>% mutate(time=0)
    names(data) = c("dbh", "n", "cl", "sp", "time")
    data = data %>% mutate(sp = as.integer(factor(sp, 
                                                  levels = c("BAH","GES", "HBU", "SAH", "SEI", "UL", "WLI"))))
  } else if(is.na(initdata)) {
    data = recruitment(spdata, dnot, deltaT, PA, time=0)
  }
  
  # add crown area columns and filter for mincohortN: 
  data = data %>% 
    mutate(ca_ind = get.crown.area(dbh, sp, spdata), 
           ca_cohort = ca_ind * n)
  
  # assign canopy layers: 
  data = assign.layers(data, spdata, PA)
  
  # group cohorts, that are now in the same canopy layer: 
  data = data %>% 
    group_by(dbh, cl, sp, time) %>% 
    summarise(n = sum(n), 
              ca_ind = unique(ca_ind), 
              ca_cohort = sum(ca_cohort)) %>% 
    select(dbh, n, cl, sp, time, ca_ind, ca_cohort) %>% 
    ungroup() %>% 
    filter(n >= mincohortN) %>% 
    assign.layers(spdata, PA) #assign canopy layers anew, after filtering for mincohortN
  
  return(data)
}

get.crown.area = function(dbh, sp, spdata) {
  spdata$param1[sp] / (1 + exp(-(spdata$inflection[sp]-dbh)/spdata$steepness[sp])) + spdata$param2[sp] / (1 + exp((spdata$inflection[sp]-dbh)/spdata$steepness[sp]))
}

assign.layers = function(data, spdata, PA) {
  
  # first, update crown area
  data = data %>% 
    mutate(ca_ind = get.crown.area(dbh, sp, spdata), 
           ca_cohort = ca_ind * n) %>% 
    arrange(desc(ca_ind), cl) %>% 
    mutate(cumca = cumsum(ca_cohort), 
           cl = ceiling(cumca/PA)) # get rough cl
  
  layers = max(data$cl)
  
  if(layers > 1) {
    # split data in the rough layers
    dat.tmp = data %>% group_by(cl) %>% group_split()
    
    # split the first cohort in the bottom layer to fill the leftover open canopy space
    for(i in seq(1, layers-1)){
      tosplit = dat.tmp[[i+1]][1,]
      opencan = i*PA - last(dat.tmp[[i]]$cumca)
      split_intop = tosplit %>% mutate(n = (n * opencan) / ca_cohort)
      dat.tmp[[i]] = bind_rows(dat.tmp[[i]], split_intop) %>% 
        mutate(cl = i)
      dat.tmp[[i+1]][1,]$n = dat.tmp[[i+1]][1,]$n - split_intop$n
    }
    
    # bind back together and update crown area
    data = bind_rows(dat.tmp) %>% 
      mutate(ca_ind = get.crown.area(dbh, sp, spdata), 
             ca_cohort = ca_ind * n, 
             cumca = cumsum(ca_cohort))
  }
  
  return(data)
}

recruitment = function(spdata, dnot, deltaT, PA, time) {
  # function to generate a tibble with newly recruited trees in one model timestep
  tibble(dbh = dnot, 
         n = spdata$rec_ha * PA / 10000 * deltaT, 
         cl = NA, 
         sp = spdata$sp, 
         time = time)
}

calc.stats = function(data, time) {
  out = data %>% 
    summarise(numtrees = sum(n), 
              ba = sum((dbh/200)^2 * pi * n), 
              timb_vol = NA, 
              maxdbh = max(dbh), 
              densVLT = sum(n[dbh > 80]), 
              totcarea  = sum(ca_cohort), 
              Dstar = min(dbh[cl == 1]), 
              numcohorts = n()) %>% 
    mutate(time = time)
}

derive.dbh.distribution = function(data, time, binwidth=5) {
  dbh_dist = data %>% 
    mutate(dbhcut = cut(dbh, 
                        breaks = seq(0,300,binwidth), 
                        labels = seq(0+binwidth/2, 300-binwidth/2, binwidth))) %>% 
    group_by(dbhcut) %>% 
    summarise(n = sum(n, na.rm = T)) %>% 
    transmute(dbh = as.numeric(as.character(dbhcut)), 
              n, 
              rel_freq = n/sum(n), 
              rel_freq = replace(rel_freq, rel_freq == 0, NA), 
              time = time)
}

plot.output = function(all_cohorts_out, mainfolder) {
  
  all_cohorts_out$sp <- as.character(all_cohorts_out$sp)
  
  
  p1 = all_cohorts_out %>%
    group_by(time,sp) %>% 
    summarise(ba = sum((dbh/200)^2 * pi * n)) %>%
    group_by(time) %>% 
    mutate(sum_ba = sum(ba)) %>% 
    mutate(sp = case_when(sp == 1 ~ "BAH",
                          sp == 2 ~ "GES",
                          sp == 3 ~ "HBU",
                          sp == 4 ~ "SAH",
                          sp == 5 ~ "SEI",
                          sp == 6 ~ "UL",
                          sp == 7 ~ "WLI",
                          TRUE ~ as.character(sp)
    )) %>% 
    ggplot() + 
      geom_line(aes(x = time, y = ba, color = sp), linewidth = 2) + 
      scale_color_manual(values = c("SAH" = "#228B22",
                                    "BAH" = "#00FF00",
                                    "HBU" = "#FFA500",
                                    "GES" = "blue",
                                    "SEI" = "red",
                                    "UL" = "#17becf",
                                    "WLI" = "grey"),
                         name = "Species") +
      xlab("Time (years)") + ylab("Basal area (m²/ha)") +
      guides(color = guide_legend(override.aes = list(linewidth = 8))) +
      theme_bw() + 
      theme(panel.grid = element_blank(), 
            legend.position = "right", 
            axis.title = element_text(size = 22), 
            axis.text = element_text(size = 18), 
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 12))
    
  print(p1)
  return(p1)
  
}

