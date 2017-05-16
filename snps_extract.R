# snps_extract
threePCs_top1000_snps <- read.delim("C:/Users/WATS5I/prjct_PCA/threePCs_top1000_snps.txt")
top_1000 <- threePCs_top1000_snps[ ,1]
write.table(top_1000, "top_1000",
            quote = F,
            row.names = F,
            col.names = F)

# strip off _A etc. in Notepad
top_1000 <- read.table("C:/Users/WATS5I/prjct_PCA/top_1000", quote="\"", comment.char="")

get_snps <- function(n){
  
  for(i in seq(5, 50, 5)) {
    snps <- top_1000[1:i,1]
    write.table(snps, paste0("snps_",i),
                quote = F,
                row.names = F,
                col.names = F)
  } 
}

