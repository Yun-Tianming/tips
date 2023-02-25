rm(list = ls())
getwd()
setwd("C:/Users/Administrator/Desktop/ShapeAnalysis")

library(shapeR)

shape = shapeR("C:/Users/Administrator/Desktop/ShapeAnalysis","test2.csv")
shape = detect.outline(shape,threshold = 0.25,write.outline.w.org = F,mouse.click = F,display.images = F)
shape = generateShapeCoefficients(shape)
shape = enrich.master.list(shape)
getMeasurements(shape)
shape = enrich.master.list(shape)


P=plotWaveletShape(shape, "pop", show.angle = F, lwd = 2,lty = 1)
p=plotFourierShape(shape, "pop", show.angle = F, lwd = 3,lty = 1)


#library(eoffice)
#topptx(figure = p,filename = "耳石重建图实验.pptx")



#write.csv(shape@master.list,file = "tol1.csv")

shape = stdCoefs(shape, classes = "pop","length_cm", bonferroni = F)

est.list = estimate.outline.reconstruction(shape)
outline.reconstruction.plot(est.list, max.num.harmonics = 15)

plotFourier(shape, coef.index=1:12,class.name = "pop", useStdcoef = T)
plotWavelet(shape, level = 5, class.name = "pop", useStdcoef = TRUE)


shape = enrich.master.list(shape)


write.csv(shape@fourier.coef.std,file = "测试.csv")
write.csv(shape@wavelet.coef.std,file = "3lastBESTtolwaveok.csv")

write.csv(shape@master.list,file = "3lastBESTtol.csv")
