# Unit conversions
# names using this format: [from]_[to]
# designed such that you should just multiply; no division

# Length
inch_mm <- 25.4

# Mass
cwt_lb <- 100
ton_lb <- 2000
kg_lb <- 2.20462
lb_kg <- 1 / kg_lb
ton_kg <- ton_lb * lb_kg

# Specialty Mass
seed_lint <- 0.365 # lb seed cotton (lint + seed + trash) -> lb lint
lint_seed <- 1 / seed_lint # lb lint -> lb seed cotton (lint + seed + trash)

# Volume
gal_l <- 3.78541
l_gal <- 1 / gal_l
acreinch_ft3 <- 3630
ft3_m3 <- 0.0283168
acreinch_m3 <- acreinch_ft3 * ft3_m3

# Hydrogeology
ft_mhead <- 0.3048 # think hydraulic lift
psi_mhead <- 0.703448 # m_head/psi
hammmhead_MJ <- 0.0979 # MJ/ha mm m-head (it's crazy but correct)

# Area
ha_ac <- 2.47105
ac_ha <- 1 / ha_ac
ac_sqft <- 43560

# Energy
mj_btu <- 947.81712
btu_mj <- 1 / mj_btu
mwh_mj <- 3600
mj_mwh <- 1 / mwh_mj
kwh_mj <- mwh_mj / 1000
kwh_btu <- kwh_mj * mj_btu

# Diesel Energy
diesel_gal_btu <- 137381 # Diesel (BTU/gal)
diesel_gal_mj <- diesel_gal_btu * btu_mj
diesel_btu_gal <- 1 / diesel_gal_btu
diesel_mj_gal <- 1 / diesel_gal_mj

# B100 Biodiesel Energy
biodiesel_gal_btu <- 119550 # BTU/gal
biodiesel_gal_mj <- biodiesel_gal_btu * btu_mj
biodiesel_btu_gal <- 1 / biodiesel_gal_btu
biodiesel_mj_gal <- 1 / biodiesel_gal_mj

# Gasoline Energy
gasoline_gal_btu <- 112114 # Gasoline (BTU/gal)
gasoline_gal_mj <- gasoline_gal_btu * btu_mj
gasoline_btu_gal <- 1 / gasoline_gal_btu
gasoline_mj_gal <- 1 / gasoline_gal_mj

# LPG Energy (Propane)
lpg_gal_btu <- 84250 # Diesel (BTU/gal)
lpg_gal_mj <- lpg_gal_btu * btu_mj
lpg_btu_gal <- 1 / lpg_gal_btu
lpg_mj_gal <- 1 / lpg_gal_mj

# Natural gas
natgas_cf_btu <- 1020 #BTU/cubic foot
natgas_cf_mj <- natgas_cf_btu * btu_mj
natgas_btu_cf <- 1 / natgas_cf_btu
natgas_mj_cf <- 1 / natgas_cf_mj

# Global Warming Potential Defaults (AR6)
gwp_co2 <- 1
gwp_ch4_bio <- 27
gwp_ch4 <- 29.8
gwp_n2o <- 273
gwp_nf3 <- 17400
gwp_sf6 <- 25200

