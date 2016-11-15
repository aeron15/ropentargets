library(methods)
# Example code to retrieve basic information as a data frame from the evidence strings for an entire data type
# using the REST API.
# The code uses methods in class "DataTypeDataFrame" that itself uses methods in class

dataTypeName <- 'genetic_association'
# Create the class instance passing in the data type name to the constructor.
evidenceForDataType <- ropentargets::EvidenceForDataType$new(dataTypeName)
# Retrieve a list of chunks of evidence strings where each chunl contains a list of evidence strings
# that are themselves lists.
evStrChunks <- evidenceForDataType$getEvStrAll()
# Instantiate the "OpenTargetsDataTypeDataFrame" class to process the lists of evidence strings.
dataTypeDF <- OpenTargetsDataTypeDataFrame$new(evStrChunks)
# Extract the fields common to all data types as a single data frame.
datatype.dataframe <- dataTypeDF$getCommonFieldsAsDataFrame()
# Print the number of rows in the resulting data frame.
print(nrow(datatype.dataframe))
