biplot.mldm = function(object, myX = c(-4,4), myY = c(-4,4), resp.var.labels = NULL, 
                     ...){
  # gives a biplot of the MLDM model
  # INPUT: 
  #   object: result of MLDM.R
  #   myX: limits for the horizontal axis
  #   myY: limits for the vertical axis  
  #   resp.var.labels: ...
  # ----------------------------------------------------------------------
  # some constants
  K = nrow(object$coef.gamma)/2
  M = ncol(object$subjects)
  n = nrow(object$subjects)
#   Z = object$class.coord 
  Z <- matrix(c(2.21,0,
                3.21,0,
                1.66,0,
                2.66,0,
                0.16,0,
                1.16,0,
                0,0.67,
                0,1.67,
                0,1.06,
                0,2.06), 
              10,2, byrow=TRUE)
  # index0 = object$index0
  index0 = as.factor(gsub("index", "",
                          noquote(rownames(object$geesummary$mean)[1:K]) ) )    ## 20MAY2016: edit "mldm" package
  XX0 = object$XX
  smv0 = object$geesummary
  vnames0 = object$names
  p = object$p
  
  if(!any(smv0$mean[-c(1:K), 4] <= 0.05) == TRUE){
    stop("At least one of the predictor must be statistically significant!")
  }
  
  for(m in 2:(M + 1)){
    if(!any(smv0$mean[(sum(p[1:m])+1):sum(p[1:(m+1)]),4] <= 0.05) == TRUE){
      stop("At least one of the predictor in each dimension must be statistically
           significant!")
    }    
  }
  
  if(is.null(resp.var.labels)) resp.var.labels = paste(1:K)
  
  # class points and decision lines 
#   png("mldm_biplot.png", width = 600, height = 600)
  # png("mldm_responce_space.png", width = 550, height = 650)
#   png("mldm_predictor_space.png", width = 600, height = 600)
  # png(file="test.png", width=5, height=5, units="cm", res=1200)
#   png(
#     "test.png",
#     width     = 3.25,
#     height    = 3.25,
#     units     = "in",
#     res       = 1200,
#     pointsize = 4
#   )
  
  plot(Z[,1], Z[,2], col = 'green', pch = 16, cex = 1, xaxt = 'n', yaxt = 'n', 
       xlab = '', ylab = '', xlim = myX, ylim = myY, asp = 1, bty="n")
  
#   plot(Z[,1], Z[,2], cex = 1, xaxt = 'n', yaxt = 'n', type = "n", 
#        xlab = '', ylab = '', xlim = myX, ylim = myY, asp = 1, bty="n")
  
  j2 = 0
  for (j in seq(1, (K * M), M)) {
    j2 = j2 + 1
    m = which(object$resp.dim.indic[j2, ] == 1)
    a = (Z[j, m] + Z[j + 1, m])/2
    if (m == 1) {
      lines(c(a, a), c(-3, 4), pch = 15, col = "green")
      text(a, -5.5, index0[j2], family = "mono", cex = 1, 
           srt = 270)
    }
    else {
      lines(c(-3, 4), c(a, a), pch = 15, col = "green")
      text(-2, a, index0[j2], family = "mono", cex = 1)
    }
  }
  
  # add co-morbidity
  text(Z[1,1]+1.50, Z[1,2]-0.50, '11100', pos =1, cex=0.8, family="mono")
  text(Z[1,1]+0.15, Z[1,2]-0.70, '11000', pos =1, cex=0.8, family="mono", srt=90)
  text(Z[1,1]-0.70, Z[1,2]-0.50, '10000', pos =1, cex=0.8, family="mono")
  text(Z[1,1]-3.00, Z[1,2]-0.50, '00000', pos =1, cex=0.8, family="mono")
  
  text(Z[1,1]-3.00, Z[1,2]+1.60, '00010', pos =1, cex=0.8, family="mono")
  text(Z[1,1]-3.00, Z[1,2]+2.70, '00011', pos =1, cex=0.8, family="mono")
  
  text(Z[1,1]-0.70, Z[1,2]+2.70, '10011', pos =1, cex=0.8, family="mono")
  text(Z[1,1]+0.15, Z[1,2]+2.50, '11011', pos =1, cex=0.8, family="mono", srt=90)
  text(Z[1,1]+1.50, Z[1,2]+2.70, '11111', pos =1, cex=0.8, family="mono")
  
  text(Z[1,1]+1.50, Z[1,2]+1.60, '11110', pos =1, cex=0.8, family="mono")
  text(Z[1,1]+0.20, Z[1,2]+1.60, '11010', pos =1, cex=0.8, family="mono", srt=45)
  text(Z[1,1]-0.70, Z[1,2]+1.60, '10010', pos =1, cex=0.8, family="mono")
  
  # extract predictors that are found to be statistically significant
  XX = list(M)
  smv = list(2)
  vnames = list(2)
  for(m in 2:(M+1))
  {
    tmp = smv0$mean[(sum(p[1:m])+1):sum(p[1:(m+1)]), ]
    smv[[m-1]] = tmp[tmp[,4]<=0.05, ]
    rownames.sig = rownames(smv[[m-1]])
    vnames[[m-1]] = gsub("[[:digit:]]","",rownames.sig)  ## split character from number
    XX00 = XX0[[m-1]]
    if(nrow(smv[[m-1]])==1){         
      XX[[m-1]] = as.matrix(XX00[ ,vnames0[[m-1]]%in%vnames[[m-1]]])
    }else{        
      XX[[m-1]] = XX00[ ,vnames0[[m-1]]%in%vnames[[m-1]]]          
    }
  }
  
  # subject points
  N = matrix(NA,n,M)
  for (m in 2:(M+1)){
    if(nrow(smv[[m-1]])==1){
      N[,m-1] = XX[[m-1]] * smv[[m-1]][,1]
    }else{
      N[,m-1] = as.matrix(XX[[m-1]]) %*% smv[[m-1]][,1] 
    }
  }
  
  points(N[seq(1,nrow(N),K), 1], N[seq(1,nrow(N),K), 2],'p', pch = 20, cex = 0.8, 
         col = gray(0.6))
  # variable axis
  axcol = 'red'
  lbcol = 'blue'
  lbcex = 1.2
  axcex = 0.9
  
  # first the BB are made
  loc1 = match(vnames[[1]],vnames[[2]])
  loc2 = match(vnames[[2]],vnames[[1]])
  
  b = list(M)
  for (m in 2:(M+1))
  {
    b[[m-1]] = round(smv[[m-1]][,1], 2)
  }
  Q = length(unique(c(vnames[[1]],vnames[[2]])))
  Q1 = length(vnames[[1]])
  BB = matrix(NA,Q,M)
  for (j in 1:length(vnames[[1]])){
    BB[j,1] = b[[1]][j]
    if (is.na(loc1[j])) BB[j,2] = 0
    else BB[j,2] = b[[2]][loc1[j]]
  }
  q = length(vnames[[1]])
  for (j in (length(vnames[[1]])+1):(length(vnames[[1]])+length(vnames[[2]]))){
    if (is.na(loc2[(j-length(vnames[[1]]))])){
      q = q+1
      BB[q,1] = 0
      BB[q,2] = b[[2]][(j-length(vnames[[1]]))]
    } 
  }
  myvarnames = c(vnames[[1]],vnames[[2]][is.na(loc2)])
  W = cbind(as.matrix(XX[[1]]),XX[[2]][,is.na(loc2)])

  # plotting the variables
  for (j in 1:Q){
    if (all((W[,j]==0) | (W[,j]==1))){
      nX = matrix(0, 2, Q)
      nX[2,j] = 1
      # plot
      NN = nX %*% BB
      points(NN[,1], NN[,2], pch = 19, col = axcol, cex = axcex)
      lines(NN[,1], NN[,2], pch = 15, col = axcol)
      dif = NN[2,] - NN[1,]
      # the labels
      if((abs(NN[2,1]) > abs(NN[2,2]))& (NN[2,1]>0)){positie = 4}
      if((abs(NN[2,1]) > abs(NN[2,2]))& (NN[2,1]<0)){positie = 2}
      if((abs(NN[2,1]) < abs(NN[2,2]))& (NN[2,2]>0)){positie = 3}
      if((abs(NN[2,1]) < abs(NN[2,2]))& (NN[2,1]<0)){positie = 1}
      text(NN[2, 1] + dif[1] / 5, NN[2, 2] + dif[2] / 5, myvarnames[j], pos = positie, 
           col = lbcol, cex = lbcex, family="mono")
    }
    else{
      inc= seq(from = floor(min(W[,j])), to = ceiling(max(W[,j])), length.out = 7)
      r = length(inc)
      nX = matrix(0, r, Q)
      nX[,j] = inc
      
      # plot
      NN = nX %*% BB
      lines(NN[,1], NN[,2], pch = 15, col = axcol, lwd=2)
      points(NN[,1],NN[,2], pch = 19, col = axcol, cex = axcex)
#       text(NN[,1],NN[,2],as.character(round(inc, digits =1)), cex = 0.7, col = axcol, 
#            pos = 3, offset =.5)
      dif = NN[r,] - NN[(r-1),]
      
      # the labels
      if((abs(NN[r,1]) > abs(NN[r,2]))& (NN[r,1]>0)){positie = 4}
      if((abs(NN[r,1]) > abs(NN[r,2]))& (NN[r,1]<0)){positie = 2}
      if((abs(NN[r,1]) < abs(NN[r,2]))& (NN[r,2]>0)){positie = 3}
      if((abs(NN[r,1]) < abs(NN[r,2]))& (NN[r,1]<0)){positie = 1}
      text(NN[r, 1] + 2 * dif[1] / 5, NN[r, 2] + 2 * dif[2] / 5, myvarnames[j], 
           pos=positie, col = lbcol, cex = lbcex, family="mono")
    } # end else
  }
  
  dev.off()
  

  
}