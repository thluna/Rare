# Paper 1 script

library(vegan)

# load community data
comm.data <- read.csv(file = "/Users/miguelmatias/Dropbox/Rarefaction/data/comm_data.csv", sep = ";")

# load dispersal data
mobility <- read.csv(file = "~/Dropbox/Rarefaction/data/traits.csv", sep = ";")
mobility <- as.vector(unlist(mobility[2]))

# separate factors from comm data
factors <- comm.data[, 1:5]
comm.data <- comm.data[,-c(1:5)]

head(factors)

SIZE <- factors[,"SmallvsLargemetacommunity"]
SITE <- factors[,"SITE"]
DIST <- factors[,"Awayvsclosetoreef"]
META <- factors[,"Metacommunityrep"]

# commdata for each group
comm.data.motile <- comm.data[,mobility=="Motile"]
comm.data.sessile <- comm.data[,mobility=="Sessile"]

# transform to presence absence
presences <- apply(comm.data, MARGIN = c(1,2), function(x) if(x > 0) x <- 1 else x <- 0)
presences.motile <- apply(comm.data[,mobility=="Motile"], MARGIN = c(1,2), function(x) if(x > 0) x <- 1 else x <- 0)
presences.sessile <- apply(comm.data[,mobility=="Sessile"], MARGIN = c(1,2), function(x) if(x > 0) x <- 1 else x <- 0)

# calculate abundances
abundances <- apply(comm.data[,], MARGIN = 1, sum)
abundances.motile <- apply(comm.data[,mobility=="Motile"], MARGIN = 1, sum)
abundances.sessile <- apply(comm.data[,mobility=="Sessile"], MARGIN = 1, sum)

# calculate diversity
richness <- apply(presences[,], MARGIN = 1, sum)
richness.motile <- apply(presences[,mobility=="Motile"], MARGIN = 1, sum)
richness.sessile <- apply(presences[,mobility=="Sessile"], MARGIN = 1, sum)

# calculate sample size

sample.size <- min(abundances)
sample.size.motile <- min(abundances.motile)
sample.size.sessile <- min(abundances.sessile)


comm.data <- matrix(as.integer(as.matrix(comm.data)),
                    ncol = ncol(comm.data),
                    nrow = nrow(comm.data))

comm.data.motile <- matrix(as.integer(as.matrix(comm.data.motile)),
                    ncol = ncol(comm.data.motile),
                    nrow = nrow(comm.data.motile))


comm.data.sessile <- matrix(as.integer(as.matrix(comm.data.sessile)),
                    ncol = ncol(comm.data.sessile),
                    nrow = nrow(comm.data.sessile))

# calculate rarefaction
rarefied <- rarefy(comm.data, sample = sample.size)
rarefied.motile <- rarefy(comm.data.motile, sample = sample.size.motile)
rarefied.sessile <- rarefy(comm.data.sessile, sample = sample.size.sessile)


write.csv(
  data.frame("rarefied_whole(142)"=rarefied, "rarefied_motile(115)"=rarefied.motile, "rarefied_sessile(6)"=rarefied.sessile),
  file = "~/Dropbox/Rarefaction/output/rarefied.csv")