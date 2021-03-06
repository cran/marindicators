#'@title Calculates the Resource Potential of species groups
#'@description This function calculates the Biomass or Abundance of a
#'  pre-defined group of species for \eqn{j} areas and \eqn{i} years.
#'@details This indicator reflects temporal dynamics of species groups.
#'@inheritParams biomassPerTL
#'@inheritParams shannon
#'@param X A dataframe of fishery independent data derived from research vessel
#'  survey data or model output, with columns \code{YEAR}, \code{ID},
#'  \code{SPECIES}, and \code{BIOMASS} or \code{ABUNDANCE}. \code{YEAR}
#'  indicates the year the observation was recorded, \code{ID} is an area code
#'  indicating where the observation was recorded, \code{SPECIES} is a numeric
#'  code indicating the species sampled, and \code{BIOMASS}/\code{ABUNDANCE} is
#'  the corresponding biomass/abundance (stratified and corrected for
#'  catchability as required).
#'@param groups A vector indicating the species group(s) for which to calculate
#'  the indicator. If \code{groups = "ALL"}, all species will be included;
#'  otherwise, each entry must be a character string matching the name of a
#'  column in \code{species.table}.
#'@return Returns a dataframe with columns \code{ID} and \code{YEAR}, and a
#'  column \code{metric_group} (e.g., \code{BIOMASS_FINFISH}) for each entry in
#'  \code{groups}.
#'
#'  If there is no data for a given year, the indicator value will be \code{NA}
#'  for that year.
#'@family resource potential indicators
#'@family ecosystem structure and functioning indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(X)
#'data(species.table)
#'
#'# Calculate total abundance and biomass
#'resourcePotential(X, groups = "ALL", metric = "ABUNDANCE", years = c(2014:2019))
#'resourcePotential(X, groups = "ALL", metric = "BIOMASS", years = c(2014:2019))
#'
#'# Calculate biomass of trophic guilds
#'trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE", "ZOOPISCIVORE")
#'resourcePotential(X, groups = trophicguild.groups,
#'    species.table = species.table, metric = "BIOMASS", years = c(2014:2019))
#'
#'# Calculate biomass of fished groups
#'resource.groups <- c("ALL", "CLUPEIDS", "FINFISH", "FLATFISH", "FORAGE", "GADOIDS", "GROUNDFISH")
#'resourcePotential(X, groups = resource.groups, species.table = species.table,
#'    metric = "BIOMASS", years = c(2014:2019))
#'@export


resourcePotential <- function(X, groups, species.table = NULL, metric = "BIOMASS", years){
  
  uI <- unique(X$ID)
  DF <- createDataframe(uI = uI, years = years)  # create a dataframe that matches each area ID to each year
  
  for(k in 1:length(groups)){                    # loop over species groups
    X.k <- speciesGroups(X = X, group = groups[k], species.table = species.table) # subset X to the species of interest
    
    ind.k <- aggregate(X.k[metric], by = c(X.k["ID"], X.k["YEAR"]), FUN = sum)    # add up metric for the species group for each year + spatial scale
    ind.k <- merge(DF, ind.k, by = c("ID", "YEAR"), all.x = T)
    
    ind.name <- paste(metric, "_", groups[k], sep ="")                       # name indicator: metric_group
    names(ind.k) = c("ID", "YEAR", ind.name)                             
    ind.k = ind.k[order(ind.k$ID), ]                # order by ID (to match output of other functions)
    
    if(k == 1) ind = ind.k
    
    ind <- merge(ind, ind.k)
  }
  
  ind                                       # return indicator values for unique(X$YEAR) 
  
  }	
			
