#######################
# outputs
#######################

tmpdir <- file.path(tempdir(), "outputs") |> checkPath(create = TRUE)
tmpFile <- file.path(tmpdir, "temp.rds")
tempObj <- 1:10

# Can add data.frame of outputs directly into simInit call
sim <- simInit(objects = c("tempObj"),
               outputs = data.frame(objectName = "tempObj"),
               paths = list(outputPath = tmpdir))
outputs(sim) # To see what will be saved, when, what filename
sim <- spades(sim)
outputs(sim) # To see that it was saved, when, what filename

# Also can add using assignment after a simList object has been made
sim <- simInit(objects = c("tempObj"), paths = list(outputPath = tmpdir))
outputs(sim) <- data.frame(objectName = "tempObj", saveTime = 1:10)
sim <- spades(sim)
outputs(sim) # To see that it was saved, when, what filename.

# can do highly variable saving
tempObj2 <- paste("val", 1:10)
df1 <- data.frame(col1 = tempObj, col2 = tempObj2)
sim <- simInit(objects = c("tempObj", "tempObj2", "df1"),
  paths = list(outputPath = tmpdir))
outputs(sim) = data.frame(
     objectName = c(rep("tempObj", 2), rep("tempObj2", 3), "df1"),
     saveTime = c(c(1,4), c(2,6,7), end(sim)),
     fun = c(rep("saveRDS", 5), "write.csv"),
     package = c(rep("base", 5), "utils"),
     stringsAsFactors = FALSE)
# since write.csv has a default of adding a column, x, with rownames, must add additional
#   argument for 6th row in data.frame (corresponding to the write.csv function)
outputArgs(sim)[[6]] <- list(row.names = FALSE)
sim <- spades(sim)
outputs(sim)

# read one back in just to test it all worked as planned
newObj <- read.csv(dir(tmpdir, pattern = "year10.csv", full.name = TRUE))
newObj

# using saving with SpaDES-aware methods
# To see current ones SpaDES can do
.saveFileExtensions()

library(terra)
ras <- rast(ncol = 4, nrow = 5)
ras[] <- 1:20

sim <- simInit(objects = c("ras"), paths = list(outputPath = tmpdir))
outputs(sim) = data.frame(
  file = "test",
  fun = "writeRaster",
  package = "terra",
  objectName = "ras",
  stringsAsFactors = FALSE)

# outputArgs(sim)[[1]] <- list(format = "GTiff") # see ?raster::writeFormats
simOut <- spades(sim)
outputs(simOut)
newRas <- rast(dir(tmpdir, full.name = TRUE, pattern = ".tif")[1])
all.equal(newRas, ras) # Should be TRUE
# Clean up after
unlink(tmpdir, recursive = TRUE)
