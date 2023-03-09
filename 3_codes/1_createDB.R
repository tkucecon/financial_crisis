
# ------------------------------------------------------------------------------
# About this code
# download the JST data and process the data for prediction

# difference with the original paper...
# NA rows are not deleted at this point
# more variables added to the prediction
  # level of exchange rates to the US dollar
  # currency peg dummy
  # two-year growth rates of house prices
  # two-year change of mortgage loans / GDP ratio
  # two-year change of household loans / GDP ratio
  # two-year change of business loans / GDP ratio
  # two-year change of level of bank capital ratio
  # two-year change of level of bank loan to deposit ratio
  # two-year change of level of bank noncore funding ratio
# global variables constructed for all variables
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("haven")
  library("ggthemes")
  theme_set(theme_solarized())
  
  library("skimr")
  
# ------------------------------------------------------------------------------
# Download the JST data
# ------------------------------------------------------------------------------
  
  # download the JST data
  df.JST.original <- 
    read_dta("http://data.macrohistory.net/JST/JSTdatasetR5.dta")
  
  # check the entire sample size and compare with the original paper
  df.JST.original %>% 
    select(year, country, crisisJST) %>% 
    skim()

  # it seems to be updated from the BOE paper
  # ... 18 developed countries are included: recent data about Ireland is now included
  # ... 2017 data is now available
  
# ------------------------------------------------------------------------------
# preprocess the data to be consistent with the BOE paper
# ------------------------------------------------------------------------------
  
  # process the domestic variables following the definition of the BOE paper
  df.JST.domestic <- 
    df.JST.original %>% 
    # group by the country
    group_by(country) %>% 
    # define the outcome variable: 1 and 2 year before the crisis is the target
    mutate(crisis.f1 = lead(crisisJST, n = 1),
           crisis.f2 = lead(crisisJST, n = 2)) %>%
    mutate(crisis = if_else(
      crisis.f1 == 1 | crisis.f2 == 1, 1, 0
    )) %>% 
    # factorize the outcome variable: not numeric
    mutate(crisis = as.factor(crisis)) %>% 
    # define some features
    mutate(slope.dom   = ltrate - stir,         # slope of the yield curve
           tloans.pgdp = tloans / gdp,          # credit per GDP
           money.pgdp  = money / gdp,           # money per GDP
           ca.pgdp     = ca / gdp,              # current account per GDP
           dsr         = tloans * ltrate / gdp, # debt service ratio
           hpreal      = hpnom / cpi * 100,     # real house price
           tmort.pgdp  = tmort / gdp,           # mortgage loan to GDP
           thh.pgdp    = thh / gdp,             # household credit to GDP
           tbus.pgdp   = tbus / gdp             # business credit to GDP
             ) %>% 
    # define some variables as 2-year difference of GDP-ratio * 100
    mutate(
      credit.dom   = (tloans.pgdp   - lag(tloans.pgdp, n = 2)) * 100, # credit
      dsr.dom      = (dsr           - lag(dsr,         n = 2)) * 100, # debt service ratio
      iy.dom       = (iy            - lag(iy,          n = 2)) * 100, # investment
      pdebt.dom    = (debtgdp       - lag(debtgdp,     n = 2)) * 100, # public debt
      money.dom    = (money.pgdp    - lag(money.pgdp,  n = 2)) * 100, # money
      ca.dom       = (ca.pgdp       - lag(ca.pgdp,     n = 2)) * 100, # current account
      mortgage.dom = (tmort.pgdp    - lag(tmort.pgdp,  n = 2)) * 100, # mortgage loan
      thh.dom      = (thh.pgdp      - lag(thh.pgdp,    n = 2)) * 100, # household credit
      tbus.dom     = (tbus.pgdp     - lag(tbus.pgdp,   n = 2)) * 100  # business credit
    ) %>% 
    # define some variables as 2-year growth rate of index
    mutate(pi.dom     = cpi    / lag(cpi,    n = 2) * 100 - 100,           # growth rate of the price level
           rcon.dom   = rconpc / lag(rconpc, n = 2) * 100 - 100,           # growth rate of the consumption
           equity.dom = ((1 + eq_tr) * (1 + lag(eq_tr, n = 1)) - 1) * 100, # growth rate of the equity price
           hpreal.dom = hpreal / lag(hpreal, n = 2) * 100 - 100            # growth rate of the real house prices
           ) %>% 
    # define some variables as 2-year difference of levels
    mutate(lev.dom     = lev     - lag(lev,     n = 2), # bank leverage
           noncore.dom = noncore - lag(noncore, n = 2)  # bank noncore funding ratio
    ) %>% 
    # rename short term interest rates
    rename(stir.dom = stir) %>% 
    # create a dummy to exclude the crisis year and four subsequent years
    mutate(crisis.ex = if_else(
      crisisJST == 1 | 
        lag(crisisJST, n = 1) == 1 | 
        lag(crisisJST, n = 2) == 1 | 
        lag(crisisJST, n = 3) == 1 | 
        lag(crisisJST, n = 4) == 1,
      1, 0
    )) %>% 
    # exclude the crisis year and the four subsequent years
    filter(crisis.ex == 0) %>% 
    # exclude some unusual periods
    filter(!(year >= 1933 & year <= 1939)) %>% # exclude the great depression period
    filter(!(year >= 1914 & year <= 1918)) %>% # exclude the WW1 period
    filter(!(year >= 1939 & year <= 1945)) %>%  # exclude the WW2 period
    # reset the grouping
    ungroup() %>% 
    # keep only the relevant variables
    select(year, country, crisis, ends_with(".dom")) 
  
  # df.JST.domestic now includes only the domestic data
  # calculate global variables
  # ... original paper uses global yield curve slope and global credit
  # ... but here I add more variables for later use
  
  # obtain the name of all the countries in the data
  countries.JST <- 
    df.JST.domestic %>% 
    select(country) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  variables.JST <- 
    df.JST.domestic %>% 
    select(ends_with(".dom")) %>% 
    colnames()

  # create an empty data frame to stack the data
  df.JST.global <- 
    df.JST.domestic %>% 
    rename_with(\(x) str_replace(x, ".dom", ".glo"),
                ends_with(".dom")) %>% 
    select(-crisis) %>% 
    filter(country == "hogehoge")
  
  # repeat the process for all the countries
  for (target.country in countries.JST) {
    # create global variables as the average of other countries
    df.JST.global.tmp <- 
      # the base data is domestic data
      df.JST.domestic %>% 
      # exclude the target country from the data
      filter(country != target.country) %>% 
      # group by each year
      group_by(year) %>% 
      # calculate the global factor as the average of other countries
      summarise_at(variables.JST, mean, na.rm = TRUE) %>% 
      # leave the name of the target country
      mutate(country = target.country) %>% 
      ungroup() %>% 
      rename_with(\(x) str_replace(x, ".dom", ".glo"),
                  ends_with(".dom"))

    # stack the data into the global data frame
    df.JST.global <- 
      bind_rows(df.JST.global, df.JST.global.tmp)  
  }
  
  # merge the domestic and global data into a single master data frame
  df.JST <- 
    df.JST.domestic %>% 
    # keep only the samples available in the domestic data frame: left join
    left_join(df.JST.global, by = c("year", "country"))
  
  # drop if the crisis is NA
  df.JST <- 
    df.JST %>% 
    filter(!is.na(crisis))
  
  # done! this procedure leaves us 1814 observations
  # if we delete the NA rows of the relevant data, 
  # this will shrink to 1260 observations (consistent with the BOE paper)
  
  # save the data
  save(df.JST, file = "../4_data/df_JST.rda")
  