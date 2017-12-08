set.seed(3)
f = function(){1.3*rnorm(4000)}
pcpDat = data.frame(ID = paste0("ID", 1:4000), A=f(), B=f(), C=f(), D=f(), E=f(), F=f())
pcpDat$ID = as.character(pcpDat$ID)
plotPCP(pcpDat = pcpDat)

