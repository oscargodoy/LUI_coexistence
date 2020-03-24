#beep alert (stop pressing escape)
library(beepr)
a <- TRUE
while (isTRUE(a)){
  Sys.sleep(2)
  beep(4)
}