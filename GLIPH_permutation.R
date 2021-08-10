#!/usr/bin/env Rscript

# Author: David M. Louis
# Contact: dmlouis@stanford.edui
# Contact: dmlouis87@gmail.com

############################### Libraries ###############################

# library(RColorBrewer)
# library(gplots)

############################### Arguments ###############################

# expected format of input file is
cmd_args = commandArgs(trailingOnly = TRUE);
# midread_table_file = cmd_args[6] ???????????

input_file=read.csv(cmd_args, header=T)
# tableLength=length(midread_table[,1])
# print(cmd_args) #prints file name
print(paste0("input length: ", length(rownames(input_file))))
#midread_matrix=data.matrix(midread_table)

############################################################################################
# Analysis Functions
############################################################################################

permutations <- function(spec_data, simulation_counts){
  spec_data$rand_add <- NA
  spec_data$rand_disease_spec <- NA
  nreps <- simulation_counts
  disease_simulations <- numeric(nreps)
  for(i in 1:nreps){
    disease_simulations[i] <- mean(
      ifelse(
        sample(
          c(colnames(spec_data[2]), colnames(spec_data[3])),
          nrow(spec_data), replace = T
        ) == colnames(spec_data)[2], spec_data[,6], spec_data[,7]
      )
    )
  }
  return(disease_simulations)
}

permutation_plot <- function(OG_file, perm_data) {
  observe1 <- mean(OG_file[,6]) #input[2]
  observe2 <- mean(OG_file[,7]) #input[3]
  plot(density(perm_data), main = "Permutation Distribution of Specificity", xlab = NA, #xlab = "Simulated, pvalue=0.00",
       xlim=c(0, 100))#max(db_perm_abs, observe2)))
  abline(v=observe1)
  abline(v=observe2)
  text(observe1, max(density(perm_data)$y)/2, colnames(OG_file[2]), srt=90, pos = 3) #srt=90, pos = 3
  text(observe2, max(density(perm_data)$y)/2, colnames(OG_file[3]), srt=90, pos = 3) #srt=90, pos = 3
  perm_plot <- recordPlot()
  return(perm_plot)
}

#Function ends

############################### Analysis  #################################

permutation_simulation <- permutations(input_file, 1000)

############################### Outputs #################################

#creating directory
mainDir <- "specificity_analysis"
dir.create(file.path(mainDir), showWarnings = FALSE)
setwd(file.path(mainDir))

############## exporting data and figures
#correlation plot
png("disease_simulation_hist.png",height=400,width=600)
hist(permutation_simulation)
dev.off()

#permutation plot
png("permutation_plot.png",height=400,width=600)
permutation_plot(input_file, permutation_simulation)
dev.off()


########## pvalues (still figuring implentation) #########
# #how many simulated means are greater than the actual mean
# sum(permutation_simulation > mean(input_file[,6]))
# sum(permutation_simulation > mean(input_file[,7]))
# #### calculating p-value
# sum(disease_simulations > mean(input_file[,6]))/nreps
# sum(disease_simulations > mean(input_file[,7]))/nreps


