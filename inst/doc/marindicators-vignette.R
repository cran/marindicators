## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE-------------------------------------------------
library(marindicators)

n.show <- 4
n.digits <- 2

## ---- echo = FALSE-------------------------------------------------------
X1 <- X[X$YEAR == 2014 & X$ID == "AREA1", ]
X1 <- X1[1:n.show,]

X2 <- X[X$YEAR == 2014 & X$ID == "AREA2", ]
X2 <- X2[1:n.show, ]

X3 <- X[X$YEAR == 2015 & X$ID == "AREA1", ]
X3 <- X3[1:n.show, ]

X4 <- X[X$YEAR == 2015 & X$ID == "AREA2", ]
X4 <- X4[1:n.show, ]

X_show <- rbind(X1, X2 ,X3, X4)

X_show$BIOMASS <- formatC(X_show$BIOMASS, format = "e", digits = n.digits)
X_show$ABUNDANCE <- formatC(X_show$ABUNDANCE, format = "e", digits = n.digits)

X_show

## ---- echo = FALSE-------------------------------------------------------
X_length <- X_length[-which(X_length$LENGTH == -99), ]

X1_length <- X_length[X_length$YEAR == 2014 & X_length$ID == "AREA1", ]
X1_length <- X1_length[1:n.show,]

X2_length <- X_length[X_length$YEAR == 2014 & X_length$ID == "AREA2", ]
X2_length <- X2_length[1:n.show, ]

X3_length <- X_length[X_length$YEAR == 2015 & X_length$ID == "AREA1", ]
X3_length <- X3_length[1:n.show, ]

X4_length <- X_length[X_length$YEAR == 2015 & X_length$ID == "AREA2", ]
X4_length <- X4_length[1:n.show, ]

X_length_show <- rbind(X1_length, X2_length ,X3_length, X4_length)

X_length_show$BIOMASS <- formatC(X_length_show$BIOMASS, format = "e", digits = n.digits)
X_length_show$ABUNDANCE <- formatC(X_length_show$ABUNDANCE, format = "e", digits = n.digits)
X_length_show

## ---- echo = FALSE-------------------------------------------------------
land <- land[order(land$YEAR),]

land1 <- land[land$YEAR == 2014 & land$ID == "AREA1", ]
land1 <- land1[1:n.show,]

land2 <- land[land$YEAR == 2014 & land$ID == "AREA2", ]
land2 <- land2[1:n.show, ]

land3 <- land[land$YEAR == 2015 & land$ID == "AREA1", ]
land3 <- land3[1:n.show, ]

land4 <- land[land$YEAR == 2015 & land$ID == "AREA2", ]
land4 <- land4[1:n.show, ]

land_show <- rbind(land1, land2 ,land3, land4)

land_show$CATCH <- formatC(land_show$CATCH, format = "e", digits = n.digits)
land_show

## ---- echo = FALSE-------------------------------------------------------
TL.table <- na.omit(species.info[, c("SPECIES", "TL")])
TL.table$TL <- round(TL.table$TL, digits = 3)
TL.table <- TL.table[1:(n.show*2), ]
TL.table

## ---- echo = FALSE-------------------------------------------------------
species.info_show <- species.info[1:10, ]
species.info_show$TL <- round(species.info_show$TL, digits = 3)
species.info_show$TL_LAND <- round(species.info_show$TL_LAND, digits = 3)
species.info_show

## ---- echo = FALSE-------------------------------------------------------
Length_Weight1 <- Length_Weight[Length_Weight$YEAR == 2014 & Length_Weight$ID == "AREA1", ]
Length_Weight1 <- Length_Weight1[1:n.show,]

Length_Weight2 <- Length_Weight[Length_Weight$YEAR == 2014 & Length_Weight$ID == "AREA2", ]
Length_Weight2 <- Length_Weight2[1:n.show, ]

Length_Weight3 <- Length_Weight[Length_Weight$YEAR == 2015 & Length_Weight$ID == "AREA1", ]
Length_Weight3 <- Length_Weight3[1:n.show, ]

Length_Weight4 <- Length_Weight[Length_Weight$YEAR == 2015 & Length_Weight$ID == "AREA2", ]
Length_Weight4 <- Length_Weight4[1:n.show, ]

Length_Weight_show <- rbind(Length_Weight1, Length_Weight2 ,Length_Weight3, Length_Weight4)

Length_Weight_show

## ---- echo = FALSE-------------------------------------------------------
species.table_show <- species.table[1:(n.show*2), c(1:7)]

species.table_show

## ---- eval = FALSE-------------------------------------------------------
#  # Compiled Data
#  data(X)
#  data(land)
#  data(species.table)
#  data(species.info)

## ------------------------------------------------------------------------
# Calculate indicators:
# Biomass of community and groundfish species
biomass <- resourcePotential(X = X, groups = c("ALL", "GROUNDFISH"), 
                             species.table = species.table, years = c(2014:2019))
biomass

# Margalef's Species Richness of community and groundfish species
marg <- margalef(X= X, groups = c("ALL", "GROUNDFISH"), metric = "ABUNDANCE",  
                 species.table = species.table, years = c(2014:2019))
marg

# Biomass per Trophic Level
biomass_TL <- biomassPerTL(X = X, TL.table = species.info, metric = "BIOMASS", 
                           TL.grouping = 1, years = c(2014:2019))
biomass_TL

# Intrinsic Vulnerability Index
IVI <- IVILandings(land = land, IVI.table = species.info, years = c(2014:2019))
IVI

# Trophic Level of Landings
TL_land <- meanTLLandings(land = land, TL.table = species.info, minTL = 0, years = c(2014:2019))
TL_land

# Marine Trophic Index
MTI_land <- meanTLLandings(land = land, TL.table = species.info, minTL = 3.25, years = c(2014:2019))
MTI_land


## ---- eval = FALSE-------------------------------------------------------
#  # Compiled Data
#  data(X)

## ------------------------------------------------------------------------
# Calculate raw indicators
allBiodiversity(X = X, metric = "ABUNDANCE", group = "ALL", years = c(2014:2019),
                percentiles = c(.25, 0.75), minTL = 0, TL.table = species.info, 
                raw = TRUE, std = FALSE, 
                export.path = NULL, glob.env = TRUE)

## ------------------------------------------------------------------------
# Calculate standardized indicators
allBiodiversity(X = X, metric = "ABUNDANCE", group = "ALL", years = c(2014:2019),
                percentiles = c(.25, 0.75), minTL = 0, TL.table = species.info, 
                raw = FALSE, std = TRUE,
                export.path = NULL, glob.env = TRUE)

## ------------------------------------------------------------------------
# Compiled data
data(X)
data(X_length)
data(land)
data(Length_Weight)
data(species.info)
data(species.table)

# Species groups of interest
resource.groups <- c("ALL", "GROUNDFISH")
ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE")
condition.groups <- c("FINFISH", "LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE", "ZOOPISCIVORE")
landings.groups <- c("ALL", "CLUPEIDS.L", "FLATFISH.L", "GROUNDFISH.L")
FP.groups <- data.frame(rbind(c("ALL", "ALL"), 
   c("CLUPEIDS", "CLUPEIDS.L"),
   c("FLATFISH", "FLATFISH.L"),
   c("GROUNDFISH", "GROUNDFISH.L")))
names(FP.groups) <- c("group.X", "group.land")

# Calculate indicators
all_inds <- extractAll(X = X, X_length = X_length, land = land, years = c(2014:2019), 
                       speciesinfo.table = species.info, species.table = species.table, 
                       metric.bio = "ABUNDANCE", group.bio = "ALL", minTL.bio = 3,
                       LSI.group = "ALL", max.length = 85, 
                       LFI.group = "ALL", large.fish = 35,
                       LenWt.table = Length_Weight,
                       guild.groups = trophicguild.groups, 
                       condition.groups = condition.groups, 
                       ratio.groups = ratio.groups,
                       maxlength.group = "FINFISH",  TL.grouping = 1, wind = 5, negative = TRUE,
                       resource.groups = resource.groups,
                       minTL.FiB = 0, base.start = 2014, base.end = 2015,
                       landings.groups = landings.groups, FP.groups = FP.groups, 
                       minTL.FP = c(0, 3.25), 
                       export.path = NULL, glob.env = TRUE,
                       raw = FALSE, std = TRUE)

names(all_inds)


