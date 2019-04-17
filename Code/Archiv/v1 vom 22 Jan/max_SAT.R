A_makro<-cbind(A,rbind(matrix(0,nrow(el),2),diag(1,nrow(r)),matrix(0,nrow(A)-2-nrow(el),2)))
dir_makro<-dir
b_makro<-b
omega_makro<-c(omega,1,1)

res_makro<-lp(direction="min", objective.in=omega_makro, const.mat=A_makro, const.dir=dir_makro, const.rhs=b_makro, all.bin=TRUE)

