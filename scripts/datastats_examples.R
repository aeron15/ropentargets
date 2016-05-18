library(ropentargets)
# Create instance of class that provides methods for obtaining information on
# data types and data sources.
restStats <- ropentargets::DataStats$new()
# Print a list that provides information on all data types. This is a big list!
print(restStats$getAllStatsAsList())
# Create a local list for inspection.
allStatsAsList <- restStats$getAllStatsAsList()

