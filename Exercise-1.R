## Read the data set into R
refine_df <- read.csv(file.choose()) # prompts you to choose the file to be loaded
refine_df$company
newCompanyNames <- function(x)
{
x <- tolower(x)
col_length <- nchar(x)
col_length
first_2 <- substr(x, 1,2)
last_2 <- substr(x, col_length-1, col_length)
if(last_2 == 'ps')
{
  return('philips')
} else if(first_2 == 'ak')
{
  return('akzo')
}else if(first_2 == 'va')
{
  return('van houten')
}else if(first_2 == 'un')
  {return('unilever')}
}
newCompanyNames(refine_df$company)
refine_df$company <- sapply(refine_df$company, FUN = newCompanyNames)
refine_df$company <- factor(refine_df$company)
refine_df$company
colnames(refine_df) # to find the column names of a dataframe

library(tidyr)
separate(refine_df, Product.code...number, c("product_code", "product_number"), sep="-")

copy_refine <- refine_df #copying original dataframe into copy_refine

#adds a column with NA values
copy_refine$prd_ctg <- rep(NA, 25)

e <- c('p = Smartphone, v = TV, x = Laptop, q = Tablet')
# Note that ==NA does not work
copy_refine$prd_ctg[is.na(copy_refine$prd_ctg)] <- e
names(copy_refine)[names(copy_refine) == 'prd_ctg'] <- 'product_category'
copy_refine

unite(copy_refine, "full_address", address, city, country, sep=",")

refine_df <- copy_refine # copying modified into the original dataframe
refine_df
