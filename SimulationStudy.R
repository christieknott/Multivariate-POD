# Distribution Statement A: Approved for public release. Distribution unlimited.
# Title: R Code accompaniment for journal article, "Multivariate Probability of Detection Modeling Including Categorical Variables and Higher-order Response Models" by Knott, Schubert Kabban and Aldrin
# Code Author: Christine Knott, PhD, Air Force Research Laboratory, RXNW
# Disclaimer: This code is intended for research use only and should be used at your own risk. 

library(survival)

# Change this to the location where you'd like the output data saved. 
outputLocation =  "C:/Temp/"


f_a = function(a){a}
f_a_i = function(a){a}

# These are the functions and terms used for the simulation in the paper. 
y_dec=3
orig_data = data.frame(
  x=rep(seq(0.02,1,1/50),2),
  material=c(rep("A",50),rep("B",50))
)

# This loop will create simulated data, then fit models. The simulated data and the models will be saved to your OutputLocation folder.
num_sims = 2 # Change this number to increase the number of simulations to run.
sim_num=1
for(sim_num in 1:num_sims){
    print(paste("Sim Number",sim_num))
    pod=data.frame(sim_num = rep(sim_num,101))
    
    # These are the functions and terms used for the simulation in the paper.     
    sim_data_A = subset(orig_data,material=="A")
    sim_data_B = subset(orig_data,material=="B")

    # If you would only like 1 repeatable, simulation, you may choose to set a seed.
    #set.seed(100)
    sim_data_A$y = with(sim_data_A,10*x^2+2)+rnorm(50,0,1)
    sim_data_B$y = with(sim_data_B,20*x^2+1)+rnorm(50,0,1)
    
    sim_data = rbind(sim_data_A,sim_data_B)
    sim_data$sim_num=sim_num
    sim_data_A$sim_num=sim_num
    sim_data_B$sim_num=sim_num
    
    data = sim_data
    data_A = subset(data,material=="A")
    data_B = subset(data,material=="B")
    
    # Run collapsed x^2 model___________________________________________________________
    model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
    a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
    a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
    a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
    a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
    x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
    U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
    U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
    V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
    V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
    a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
    a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
    a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
    a50=NA;a90=NA;a9095=NA;
    
    mat="Both"
    model.name="collapsed.x2"
    pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                          a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                          sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
    sim_data = data
    model.df = NULL
    sim_data$model.name=model.name
    sim_data$mat=mat
    
    #Collapsed Model 
    print("Collapsed x^2+x model_____________________")
    model.collapsed.x2 = survreg(formula = Surv(y) ~ poly(x,2), dist = "gaussian", data = sim_data)
    summary(model.collapsed.x2)
    vcov.x.collapsed = vcov(model.collapsed.x2)
    
    print("Collapsed z=x^2+x model_____________________")
    sim_data$z  = predict(model.collapsed.x2)
    model.z2     = survreg(formula = Surv(y) ~ z, dist = "gaussian", data = sim_data)
    summary(model.z2)
    vcov.z = vcov(model.z2)
    
    x_mod       = model.collapsed.x2
    z_mod       = model.z2
    x_vals      = sim_data$x
    x_mod_vcov	=	vcov(x_mod)					
    z_mod_vcov	=	vcov(z_mod)					
    
    a0=x_mod$coefficients[[1]]
    a1=x_mod$coefficients[[2]]
    a2=x_mod$coefficients[[3]]
    b0=z_mod$coefficients[[1]]
    b1=z_mod$coefficients[[2]]
    
    a0_var	     = x_mod_vcov[[1,1]]					
    a1_var	     = x_mod_vcov[[2,2]]					
    a2_var	     = x_mod_vcov[[3,3]]					
    a.sigma_var	 = x_mod_vcov[[4,4]]					
    a0_a1_cov		 = x_mod_vcov[[1,2]]					
    a0_a2_cov		 = x_mod_vcov[[1,3]]					
    a1_a2_cov		 = x_mod_vcov[[2,3]]					
    a0_sigma_cov = x_mod_vcov[[1,4]]					
    a1_sigma_cov = x_mod_vcov[[2,4]]					
    a2_sigma_cov = x_mod_vcov[[3,4]]					
    b0_var		   = z_mod_vcov[[1,1]]					
    b1_var		   = z_mod_vcov[[2,2]]					
    b.sigma_var	 = z_mod_vcov[[3,3]]					
    b0_b1_cov		 = z_mod_vcov[[1,2]]					
    b0_sigma_cov = z_mod_vcov[[1,3]]					
    b1_sigma_cov = z_mod_vcov[[2,3]]					
    
    x_max = max(data$x)
    sim_data$var_mult = a0_var+(x_vals^2)*a1_var+(x_vals^4)*a2_var+2*(x_vals)*a0_a1_cov+2*(x_vals^2)*a0_a2_cov+2*(x_vals^2)*a1_a2_cov							
    var_mult_max      = a0_var+(x_max^2)*a1_var+(x_max^4)*a2_var+2*(x_max)*a0_a1_cov+2*(x_max^2)*a0_a2_cov+2*(x_max^3)*a1_a2_cov							
    sigma_max = sqrt(var_mult_max) # This occurs at the maximum of x. 
    varOfvar  = var(sim_data$var_mult)
    
    mu.pod    = (y_dec-b0)/b1
    sigma.pod = sigma_max
    
    U=matrix(c(-1/b1,-mu.pod/b1,0,0,0,1/(2*b1*sigma_max)),nrow=3)
    V=z_mod_vcov
    V_POD = t(U)%*%V%*%U
    
    U_1_1 = U[1,1]
    U_1_2 = U[1,2]
    U_2_1 = U[2,1]
    U_2_2 = U[2,2]
    U_3_1 = U[3,1]
    U_3_2 = U[3,2]
    
    V_1_1 = V[1,1]
    V_1_2 = V[1,2]
    V_1_3 = V[1,3]
    V_2_1 = V[2,1]
    V_2_2 = V[2,2]
    V_2_3 = V[2,3]
    V_3_1 = V[3,1]
    V_3_2 = V[3,2]
    V_3_3 = V[3,3]
    
    V_POD_1_1 = V_POD[1,1]
    V_POD_1_2 = V_POD[1,2]
    V_POD_2_1 = V_POD[2,1]
    V_POD_2_2 = V_POD[2,2]
    
    a50.z       = qnorm(0.5)*sigma.pod+mu.pod
    a90.z       = qnorm(0.9)*sigma.pod+mu.pod
    a90.sigma.z = sqrt(V_POD_1_1+2*qnorm(0.9)*V_POD_1_2+qnorm(0.9)^2*V_POD_2_2)
    a90.95.z    = a90.z+qnorm(0.95)*a90.sigma.z
    
    z=a50.z
    a50_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    a50_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    z=a90.z
    a90_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    a90_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    z=a90.95.z
    a9095_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    a9095_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    
    pod$a.p.z       = qnorm(pod$probability)*sigma.pod+mu.pod
    pod$a.p.sigma.z = sqrt(V_POD_1_1+2*qnorm(pod$probability)*V_POD_1_2+qnorm(pod$probability)^2*V_POD_2_2)
    pod$a.p.q.z     = pod$a.p.z+qnorm(0.95)*pod$a.p.sigma.z
    
    pod$a.p_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.z)))/(2*b1*a2)
    pod$a.p_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.z)))/(2*b1*a2)
    pod$a.p.q_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.q.z)))/(2*b1*a2)
    pod$a.p.q_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.q.z)))/(2*b1*a2)
    
    pod$a.p       = pod$a.p_2
    pod$a.p.sigma = pod$a.p.sigma.z
    pod$a.p.q     = pod$a.p.q_2
    
    model.df=data.frame(sim_num,model.name,mat,a0,a1,a2,b0,b1,a0_var,a1_var,a2_var,a.sigma_var,a0_a1_cov,a0_a2_cov,a1_a2_cov,a0_sigma_cov,a1_sigma_cov,a2_sigma_cov,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,x_max,var_mult_max,sigma_max,varOfvar,mu.pod,sigma.pod,U_1_1,U_1_2,U_2_1,U_2_2,U_3_1,U_3_2,V_1_1,V_1_2,V_1_3,V_2_1,V_2_2,V_2_3,V_3_1,V_3_2,V_3_3,V_POD_1_1,V_POD_1_2,V_POD_2_1,V_POD_2_2,a50.z,a90.z,a90.sigma.z,a90.95.z,a50_1,a50_2,a90_1,a90_2,a9095_1,a9095_2)
    model.df=c(model.df,glance(x_mod))
    
    # Save dataframes
    if(sim_num==1){
      write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=TRUE,sep=",")
      write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=TRUE,sep=",")
      write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=TRUE,sep=",")
    }else{
      write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
    }
    
    
    # Run Model for Material A Data Only ________________________________________________________
    model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
    a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
    a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
    a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
    a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
    x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
    U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
    U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
    V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
    V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
    a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
    a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
    a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
    a50=NA;a90=NA;a9095=NA;
    
    sim_data = data_A
    model.df = NULL
    mat="A"
    model.name="collapsed.x2: A"
    sim_data_A$model.name=model.name
    sim_data_A$mat=mat
    pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                          a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                          sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
    #Collapsed Model 
    print("Collapsed x^2+x model for A_____________________")
    model.x.collapsed2A = survreg(formula = Surv(y) ~ poly(x,2), dist = "gaussian", data = sim_data_A)
    summary(model.x.collapsed2A)
    vcov.x.collapsed = vcov(model.x.collapsed2A)
    
    print("Collapsed z=x^2+x model for A_____________________")
    sim_data_A$z  = predict(model.x.collapsed2A)
    model.z2     = survreg(formula = Surv(y) ~ z, dist = "gaussian", data = sim_data_A)
    summary(model.z2)
    vcov.z = vcov(model.z2)
    
    x_mod       = model.x.collapsed2A
    z_mod       = model.z2
    x_vals      = sim_data_A$x
    x_mod_vcov	=	vcov(x_mod)					
    z_mod_vcov	=	vcov(z_mod)					
    
    a0=x_mod$coefficients[[1]]
    a1=x_mod$coefficients[[2]]
    a2=x_mod$coefficients[[3]]
    b0=z_mod$coefficients[[1]]
    b1=z_mod$coefficients[[2]]
    
    a0_var	     = x_mod_vcov[[1,1]]					
    a1_var	     = x_mod_vcov[[2,2]]					
    a2_var	     = x_mod_vcov[[3,3]]					
    a.sigma_var	 = x_mod_vcov[[4,4]]					
    a0_a1_cov		 = x_mod_vcov[[1,2]]					
    a0_a2_cov		 = x_mod_vcov[[1,3]]					
    a1_a2_cov		 = x_mod_vcov[[2,3]]					
    a0_sigma_cov = x_mod_vcov[[1,4]]					
    a1_sigma_cov = x_mod_vcov[[2,4]]					
    a2_sigma_cov = x_mod_vcov[[3,4]]					
    b0_var		   = z_mod_vcov[[1,1]]					
    b1_var		   = z_mod_vcov[[2,2]]					
    b.sigma_var	 = z_mod_vcov[[3,3]]					
    b0_b1_cov		 = z_mod_vcov[[1,2]]					
    b0_sigma_cov = z_mod_vcov[[1,3]]					
    b1_sigma_cov = z_mod_vcov[[2,3]]					
    
    x_max = max(data$x)
    sim_data_A$var_mult = a0_var+(x_vals^2)*a1_var+(x_vals^4)*a2_var+2*(x_vals)*a0_a1_cov+2*(x_vals^2)*a0_a2_cov+2*(x_vals^2)*a1_a2_cov							
    var_mult_max      = a0_var+(x_max^2)*a1_var+(x_max^4)*a2_var+2*(x_max)*a0_a1_cov+2*(x_max^2)*a0_a2_cov+2*(x_max^3)*a1_a2_cov							
    sigma_max = sqrt(var_mult_max) # This occurs at the maximum of x. 
    varOfvar  = var(sim_data_A$var_mult)
    
    mu.pod    = (y_dec-b0)/b1
    sigma.pod = sigma_max
    
    U=matrix(c(-1/b1,-mu.pod/b1,0,0,0,1/(2*b1*sigma_max)),nrow=3)
    V=z_mod_vcov
    V_POD = t(U)%*%V%*%U
    
    U_1_1 = U[1,1]
    U_1_2 = U[1,2]
    U_2_1 = U[2,1]
    U_2_2 = U[2,2]
    U_3_1 = U[3,1]
    U_3_2 = U[3,2]
    
    V_1_1 = V[1,1]
    V_1_2 = V[1,2]
    V_1_3 = V[1,3]
    V_2_1 = V[2,1]
    V_2_2 = V[2,2]
    V_2_3 = V[2,3]
    V_3_1 = V[3,1]
    V_3_2 = V[3,2]
    V_3_3 = V[3,3]
    
    V_POD_1_1 = V_POD[1,1]
    V_POD_1_2 = V_POD[1,2]
    V_POD_2_1 = V_POD[2,1]
    V_POD_2_2 = V_POD[2,2]
    
    a50.z       = qnorm(0.5)*sigma.pod+mu.pod
    a90.z       = qnorm(0.9)*sigma.pod+mu.pod
    a90.sigma.z = sqrt(V_POD_1_1+2*qnorm(0.9)*V_POD_1_2+qnorm(0.9)^2*V_POD_2_2)
    a90.95.z    = a90.z+qnorm(0.95)*a90.sigma.z
    
    
    z=a50.z
    a50_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    a50_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    z=a90.z
    a90_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    a90_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    z=a90.95.z
    a9095_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    a9095_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
    
    pod$a.p.z       = qnorm(pod$probability)*sigma.pod+mu.pod
    pod$a.p.sigma.z = sqrt(V_POD_1_1+2*qnorm(pod$probability)*V_POD_1_2+qnorm(pod$probability)^2*V_POD_2_2)
    pod$a.p.q.z     = pod$a.p.z+qnorm(0.95)*pod$a.p.sigma.z
    
    pod$a.p_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.z)))/(2*b1*a2)
    pod$a.p_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.z)))/(2*b1*a2)
    pod$a.p.q_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.q.z)))/(2*b1*a2)
    pod$a.p.q_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.q.z)))/(2*b1*a2)
    
    pod$a.p       = pod$a.p_2
    pod$a.p.sigma = pod$a.p.sigma.z
    pod$a.p.q     = pod$a.p.q_2
    
    model.df=data.frame(sim_num,model.name,mat,a0,a1,a2,b0,b1,a0_var,a1_var,a2_var,a.sigma_var,a0_a1_cov,a0_a2_cov,a1_a2_cov,a0_sigma_cov,a1_sigma_cov,a2_sigma_cov,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,x_max,var_mult_max,sigma_max,varOfvar,mu.pod,sigma.pod,U_1_1,U_1_2,U_2_1,U_2_2,U_3_1,U_3_2,V_1_1,V_1_2,V_1_3,V_2_1,V_2_2,V_2_3,V_3_1,V_3_2,V_3_3,V_POD_1_1,V_POD_1_2,V_POD_2_1,V_POD_2_2,a50.z,a90.z,a90.sigma.z,a90.95.z,a50_1,a50_2,a90_1,a90_2,a9095_1,a9095_2)
    model.df=c(model.df,glance(x_mod))
    
    # Save dataframes
      write.table(sim_data_A,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
    
      
  # Run Model for Material B Data Only ________________________________________________________
      model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
      a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
      a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
      a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
      a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
      x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
      U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
      U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
      V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
      V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
      a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
      a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
      a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
      a50=NA;a90=NA;a9095=NA;
      
      sim_data = data_B
      model.df = NULL
      mat="B"
      model.name="collapsed.x2: B"
      sim_data_B$model.name=model.name
      sim_data_B$mat=mat
      pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      
      #Collapsed Model 
      print("Collapsed x^2+x model for B_____________________")
      model.x.collapsed2B = survreg(formula = Surv(y) ~ poly(x,2), dist = "gaussian", data = sim_data_B)
      summary(model.x.collapsed2B)
      vcov.x.collapsed = vcov(model.x.collapsed2B)
      
      print("Collapsed z=x^2+x model for B_____________________")
      sim_data_B$z  = predict(model.x.collapsed2B)
      model.z2     = survreg(formula = Surv(y) ~ z, dist = "gaussian", data = sim_data_B)
      summary(model.z2)
      vcov.z = vcov(model.z2)
      
      x_mod       = model.x.collapsed2B
      z_mod       = model.z2
      x_vals      = sim_data_B$x
      x_mod_vcov	=	vcov(x_mod)					
      z_mod_vcov	=	vcov(z_mod)					
      
      a0=x_mod$coefficients[[1]]
      a1=x_mod$coefficients[[2]]
      a2=x_mod$coefficients[[3]]
      b0=z_mod$coefficients[[1]]
      b1=z_mod$coefficients[[2]]
      
      a0_var	     = x_mod_vcov[[1,1]]					
      a1_var	     = x_mod_vcov[[2,2]]					
      a2_var	     = x_mod_vcov[[3,3]]					
      a.sigma_var	 = x_mod_vcov[[4,4]]					
      a0_a1_cov		 = x_mod_vcov[[1,2]]					
      a0_a2_cov		 = x_mod_vcov[[1,3]]					
      a1_a2_cov		 = x_mod_vcov[[2,3]]					
      a0_sigma_cov = x_mod_vcov[[1,4]]					
      a1_sigma_cov = x_mod_vcov[[2,4]]					
      a2_sigma_cov = x_mod_vcov[[3,4]]					
      b0_var		   = z_mod_vcov[[1,1]]					
      b1_var		   = z_mod_vcov[[2,2]]					
      b.sigma_var	 = z_mod_vcov[[3,3]]					
      b0_b1_cov		 = z_mod_vcov[[1,2]]					
      b0_sigma_cov = z_mod_vcov[[1,3]]					
      b1_sigma_cov = z_mod_vcov[[2,3]]					
      
      x_max = max(data$x)
      sim_data_B$var_mult = a0_var+(x_vals^2)*a1_var+(x_vals^4)*a2_var+2*(x_vals)*a0_a1_cov+2*(x_vals^2)*a0_a2_cov+2*(x_vals^2)*a1_a2_cov							
      var_mult_max      = a0_var+(x_max^2)*a1_var+(x_max^4)*a2_var+2*(x_max)*a0_a1_cov+2*(x_max^2)*a0_a2_cov+2*(x_max^3)*a1_a2_cov							
      sigma_max = sqrt(var_mult_max) # This occurs at the maximum of x. 
      varOfvar  = var(sim_data_B$var_mult)
      
      mu.pod    = (y_dec-b0)/b1
      sigma.pod = sigma_max
      
      U=matrix(c(-1/b1,-mu.pod/b1,0,0,0,1/(2*b1*sigma_max)),nrow=3)
      V=z_mod_vcov
      V_POD = t(U)%*%V%*%U
      
      U_1_1 = U[1,1]
      U_1_2 = U[1,2]
      U_2_1 = U[2,1]
      U_2_2 = U[2,2]
      U_3_1 = U[3,1]
      U_3_2 = U[3,2]
      
      V_1_1 = V[1,1]
      V_1_2 = V[1,2]
      V_1_3 = V[1,3]
      V_2_1 = V[2,1]
      V_2_2 = V[2,2]
      V_2_3 = V[2,3]
      V_3_1 = V[3,1]
      V_3_2 = V[3,2]
      V_3_3 = V[3,3]
      
      V_POD_1_1 = V_POD[1,1]
      V_POD_1_2 = V_POD[1,2]
      V_POD_2_1 = V_POD[2,1]
      V_POD_2_2 = V_POD[2,2]
      
      a50.z       = qnorm(0.5)*sigma.pod+mu.pod
      a90.z       = qnorm(0.9)*sigma.pod+mu.pod
      a90.sigma.z = sqrt(V_POD_1_1+2*qnorm(0.9)*V_POD_1_2+qnorm(0.9)^2*V_POD_2_2)
      a90.95.z    = a90.z+qnorm(0.95)*a90.sigma.z
      
      
      z=a50.z
      a50_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
      a50_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
      z=a90.z
      a90_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
      a90_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
      z=a90.95.z
      a9095_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
      a9095_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-z)))/(2*b1*a2)
      
      pod$a.p.z       = qnorm(pod$probability)*sigma.pod+mu.pod
      pod$a.p.sigma.z = sqrt(V_POD_1_1+2*qnorm(pod$probability)*V_POD_1_2+qnorm(pod$probability)^2*V_POD_2_2)
      pod$a.p.q.z     = pod$a.p.z+qnorm(0.95)*pod$a.p.sigma.z
      
      pod$a.p_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.z)))/(2*b1*a2)
      pod$a.p_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.z)))/(2*b1*a2)
      pod$a.p.q_1=(-(b1*a1)-sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.q.z)))/(2*b1*a2)
      pod$a.p.q_2=(-(b1*a1)+sqrt(b1^2*a1^2-4*b1*a2*(b0-b1*a0-pod$a.p.q.z)))/(2*b1*a2)
      
      pod$a.p       = pod$a.p_2
      pod$a.p.sigma = pod$a.p.sigma.z
      pod$a.p.q     = pod$a.p.q_2
      
      model.df=data.frame(sim_num,model.name,mat,a0,a1,a2,b0,b1,a0_var,a1_var,a2_var,a.sigma_var,a0_a1_cov,a0_a2_cov,a1_a2_cov,a0_sigma_cov,a1_sigma_cov,a2_sigma_cov,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,x_max,var_mult_max,sigma_max,varOfvar,mu.pod,sigma.pod,U_1_1,U_1_2,U_2_1,U_2_2,U_3_1,U_3_2,V_1_1,V_1_2,V_1_3,V_2_1,V_2_2,V_2_3,V_3_1,V_3_2,V_3_3,V_POD_1_1,V_POD_1_2,V_POD_2_1,V_POD_2_2,a50.z,a90.z,a90.sigma.z,a90.95.z,a50_1,a50_2,a90_1,a90_2,a9095_1,a9095_2)
      model.df=c(model.df,glance(x_mod))
      
      # Save dataframes
      write.table(sim_data_B,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      
      
      
    # Run x^2+material model___________________________________________________________
      model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
      a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
      a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
      a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
      a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
      x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
      U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
      U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
      V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
      V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
      a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
      a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
      a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
      a50=NA;a90=NA;a9095=NA;mu.pod_A=NA;mu.pod_B=NA;
      a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;
      a50_1=NA;a50_2_B=NA;a90_1_B=NA;a90_2_B=NA;a9095_1_B=NA;a9095_2_B=NA;
      
      sim_data = data
      model.df = NULL
      mat="Both"
      model.name="x2+material"
      sim_data$model.name=model.name
      sim_data$mat=mat
      pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)      
      pod_A    = data.frame(probability = seq(0,1,0.01),model.name, mat="A", a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      pod_B    = data.frame(probability = seq(0,1,0.01),model.name, mat="B", a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      
      print("x^2+material model_____________________")
      model.x2 = survreg(formula = Surv(y) ~ poly(x,2)+material+poly(x,2):material, dist = "gaussian", data = sim_data)
      summary(model.x2)
      vcov.x = vcov(model.x2)
      
      print("z=x^2+x model_____________________")
      sim_data$z   = predict(model.x2)
      model.z2     = survreg(formula = Surv(y) ~ z, dist = "gaussian", data = sim_data)
      summary(model.z2)
      vcov.z = vcov(model.z2)
      
      x_mod = model.x2
      z_mod = model.z2
      x_vals      = sim_data$x
      x_mod_vcov	=	vcov(x_mod)					
      z_mod_vcov	=	vcov(z_mod)					
      
      a0_A=x_mod$coefficients[[1]]
      a1_A=x_mod$coefficients[[2]]
      a2_A=x_mod$coefficients[[3]]
      a0_B=x_mod$coefficients[[1]]+x_mod$coefficients[[4]]
      a1_B=x_mod$coefficients[[2]]+x_mod$coefficients[[5]]
      a2_B=x_mod$coefficients[[3]]+x_mod$coefficients[[6]]
      b0=z_mod$coefficients[[1]]
      b1=z_mod$coefficients[[2]]
    
      a0_var	     = x_mod_vcov[[1,1]]					
      a0_a1_cov		 = x_mod_vcov[[1,2]]					
      a0_a2_cov		 = x_mod_vcov[[1,3]]					
      a0_a3_cov		 = x_mod_vcov[[1,4]]					
      a0_a4_cov		 = x_mod_vcov[[1,5]]					
      a0_a5_cov		 = x_mod_vcov[[1,6]]			
      a0_sigma_cov = x_mod_vcov[[1,7]]
      a1_var	     = x_mod_vcov[[2,2]]					
      a1_a2_cov		 = x_mod_vcov[[2,3]]					
      a1_a3_cov		 = x_mod_vcov[[2,4]]					
      a1_a4_cov		 = x_mod_vcov[[2,5]]					
      a1_a5_cov		 = x_mod_vcov[[2,6]]					
      a1_sigma_cov = x_mod_vcov[[2,7]]					
      a2_var	     = x_mod_vcov[[3,3]]					
      a2_a3_cov		 = x_mod_vcov[[3,4]]					
      a2_a4_cov		 = x_mod_vcov[[3,5]]					
      a2_a5_cov		 = x_mod_vcov[[3,6]]					
      a2_sigma_cov = x_mod_vcov[[3,7]]
      a3_var     	 = x_mod_vcov[[4,4]]
      a3_a4_cov		 = x_mod_vcov[[4,5]]					
      a3_a5_cov		 = x_mod_vcov[[4,6]]					
      a3_sigma_cov = x_mod_vcov[[4,7]]
      a4_var	     = x_mod_vcov[[5,5]]					
      a4_a5_cov		 = x_mod_vcov[[5,6]]					
      a4_sigma_cov = x_mod_vcov[[5,7]]					
      a5_var	     = x_mod_vcov[[6,6]]					
      a5_sigma_cov = x_mod_vcov[[6,7]]
      a.sigma_var	 = x_mod_vcov[[7,7]]
      
      b0_var		   = z_mod_vcov[[1,1]]					
      b1_var		   = z_mod_vcov[[2,2]]					
      b.sigma_var	 = z_mod_vcov[[3,3]]					
      b0_b1_cov		 = z_mod_vcov[[1,2]]					
      b0_sigma_cov = z_mod_vcov[[1,3]]					
      b1_sigma_cov = z_mod_vcov[[2,3]]					
    
      x_max = max(data$x)
      x_vals_A = data_A$x
      x_vals_B = data_B$x
      
      var_mult_A     = a0_var+(x_vals_A^2)*a1_var+(x_vals_A^4)*a2_var+2*(x_vals_A)*a0_a1_cov+2*(x_vals_A^2)*a0_a2_cov+2*(x_vals_A^3)*a1_a2_cov
      var_mult_max_A = a0_var+   (x_max^2)*a1_var+   (x_max^4)*a2_var+   2*(x_max)*a0_a1_cov+   2*(x_max^2)*a0_a2_cov+   2*(x_max^3)*a1_a2_cov
      sigma_max_A = sqrt(var_mult_max_A) # This occurs at the maximum of x. 
      varOfvar_A  = var(var_mult_A)
      # B has extra terms since m_B = TRUE. 
      var_mult_B     = (a0_var+a3_var+2*a0_a3_cov)+(x_vals_B^2)*(a1_var+a4_var+2*a1_a4_cov)+(x_vals_B^4)*(a2_var+a5_var+2*a2_a5_cov)+2*x_vals_B*(a0_a1_cov+a0_a4_cov+a1_a3_cov+a3_a4_cov)+2*(x_vals_B^2)*(a0_a2_cov+a0_a5_cov+a2_a3_cov+a3_a5_cov)+2*(x_vals_B^3)*(a1_a2_cov+a1_a5_cov+a2_a4_cov+a4_a5_cov)
      var_mult_max_B = (a0_var+a3_var+2*a0_a3_cov)+   (x_max^2)*(a1_var+a4_var+2*a1_a4_cov)+   (x_max^4)*(a2_var+a5_var+2*a2_a5_cov)+   2*x_max*(a0_a1_cov+a0_a4_cov+a1_a3_cov+a3_a4_cov)+   2*(x_max^2)*(a0_a2_cov+a0_a5_cov+a2_a3_cov+a3_a5_cov)+   2*(x_max^3)*(a1_a2_cov+a1_a5_cov+a2_a4_cov+a4_a5_cov)
      sigma_max_B = sqrt(var_mult_max_B) # This occurs at the maximum of x. 
      varOfvar_B  = var(var_mult_B)
      # Combine
      sim_data$var_mult = c(var_mult_A,var_mult_B)
      
      mu.pod  = (y_dec-b0)/b1
      mu.pod_A  = mu.pod
      mu.pod_B  = mu.pod
      sigma.pod_A = sigma_max_A
      sigma.pod_B = sigma_max_B
      
      U_A=matrix(c(-1/b1,-mu.pod/b1,0,0,0,1/(2*b1*sigma_max_A)),nrow=3)
      V_A=matrix(c(vcov(z_mod)[1,1],vcov(z_mod)[1,2],0,vcov(z_mod)[2,1],vcov(z_mod)[2,2],0,0,0,varOfvar_A),nrow=3)
      V_POD_A = t(U)%*%V%*%U
      a50.z_A=qnorm(0.5)*sigma.pod_A+mu.pod
      a90.z_A=qnorm(0.9)*sigma.pod_A+mu.pod
      a90.sigma.z_A=sqrt(V_POD_A[1,1]+2*qnorm(0.9)*V_POD_A[1,2]+qnorm(0.9)^2*V_POD_A[2,2])
      a90.95.z_A=a90.z_A+qnorm(0.95)*a90.sigma.z_A
      
      sigma.pod_B=sigma_max_B
      U_B=matrix(c(-1/b1,-mu.pod/b1,0,0,0,1/(2*b1*sigma_max_B)),nrow=3)
      V_B=matrix(c(vcov(z_mod)[1,1],vcov(z_mod)[1,2],0,vcov(z_mod)[2,1],vcov(z_mod)[2,2],0,0,0,varOfvar_B),nrow=3)
      V_POD_B = t(U)%*%V%*%U
      a50.z_B=qnorm(0.5)*sigma.pod_B+mu.pod
      a90.z_B=qnorm(0.9)*sigma.pod_B+mu.pod
      a90.sigma.z_B=sqrt(V_POD_B[1,1]+2*qnorm(0.9)*V_POD_B[1,2]+qnorm(0.9)^2*V_POD_B[2,2])
      a90.95.z_B=a90.z_B+qnorm(0.95)*a90.sigma.z_B
      
      U_1_1_A = U_A[1,1]
      U_1_2_A = U_A[1,2]
      U_2_1_A = U_A[2,1]
      U_2_2_A = U_A[2,2]
      U_3_1_A = U_A[3,1]
      U_3_2_A = U_A[3,2]
      
      V_1_1_A = V_A[1,1]
      V_1_2_A = V_A[1,2]
      V_1_3_A = V_A[1,3]
      V_2_1_A = V_A[2,1]
      V_2_2_A = V_A[2,2]
      V_2_3_A = V_A[2,3]
      V_3_1_A = V_A[3,1]
      V_3_2_A = V_A[3,2]
      V_3_3_A = V_A[3,3]
      
      V_POD_1_1_A = V_POD_A[1,1]
      V_POD_1_2_A = V_POD_A[1,2]
      V_POD_2_1_A = V_POD_A[2,1]
      V_POD_2_2_A = V_POD_A[2,2]
      
      U_1_1_B = U_B[1,1]
      U_1_2_B = U_B[1,2]
      U_2_1_B = U_B[2,1]
      U_2_2_B = U_B[2,2]
      U_3_1_B = U_B[3,1]
      U_3_2_B = U_B[3,2]
      
      V_1_1_B = V_B[1,1]
      V_1_2_B = V_B[1,2]
      V_1_3_B = V_B[1,3]
      V_2_1_B = V_B[2,1]
      V_2_2_B = V_B[2,2]
      V_2_3_B = V_B[2,3]
      V_3_1_B = V_B[3,1]
      V_3_2_B = V_B[3,2]
      V_3_3_B = V_B[3,3]
      
      V_POD_1_1_B = V_POD_B[1,1]
      V_POD_1_2_B = V_POD_B[1,2]
      V_POD_2_1_B = V_POD_B[2,1]
      V_POD_2_2_B = V_POD_B[2,2]
      
      z=a50.z_A
      a50_1_A=(-(b1*a1_A)-sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-z)))/(2*b1*a2_A)
      a50_2_A=(-(b1*a1_A)+sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-z)))/(2*b1*a2_A)
      z=a50.z_B
      a50_1_B=(-(b1*a1_B)-sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-z)))/(2*b1*a2_B)
      a50_2_B=(-(b1*a1_B)+sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-z)))/(2*b1*a2_B)
      z=a90.z_A
      a90_1_A=(-(b1*a1_A)-sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-z)))/(2*b1*a2_A)
      a90_2_A=(-(b1*a1_A)+sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-z)))/(2*b1*a2_A)
      z=a90.z_B
      a90_1_B=(-(b1*a1_B)-sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-z)))/(2*b1*a2_B)
      a90_2_B=(-(b1*a1_B)+sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-z)))/(2*b1*a2_B)
      z=a90.95.z_A
      a9095_1_A=(-(b1*a1_A)-sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-z)))/(2*b1*a2_A)
      a9095_2_A=(-(b1*a1_A)+sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-z)))/(2*b1*a2_A)
      z=a90.95.z_B
      a9095_1_B=(-(b1*a1_B)-sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-z)))/(2*b1*a2_B)
      a9095_2_B=(-(b1*a1_B)+sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-z)))/(2*b1*a2_B)
      
      pod_A$a.p.z      =qnorm(pod_A$probability)*sigma.pod_A+mu.pod
      pod_A$a.p.sigma.z=sqrt(V_POD_1_1_A+2*qnorm(pod_A$probability)*V_POD_1_2_A+qnorm(pod_A$probability)^2*V_POD_2_2_A)
      pod_A$a.p.q.z    =pod_A$a.p.z+qnorm(0.95)*pod_A$a.p.sigma.z
      
      pod_A$a.p_1  =(-(b1*a1_A)-sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-pod_A$a.p.z)))/(2*b1*a2_A)
      pod_A$a.p_2  =(-(b1*a1_A)+sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-pod_A$a.p.z)))/(2*b1*a2_A)
      pod_A$a.p.q_1=(-(b1*a1_A)-sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-pod_A$a.p.q.z)))/(2*b1*a2_A)
      pod_A$a.p.q_2=(-(b1*a1_A)+sqrt(b1^2*a1_A^2-4*b1*a2_A*(b0-b1*a0_A-pod_A$a.p.q.z)))/(2*b1*a2_A)
      
      pod_B$a.p.z     =qnorm(pod_B$probability)*sigma.pod_B+mu.pod
      pod_B$a.p.sigma.z=sqrt(V_POD_1_1_B+2*qnorm(pod_B$probability)*V_POD_1_2_B+qnorm(pod_B$probability)^2*V_POD_2_2_B)
      pod_B$a.p.q.z    =pod_B$a.p.z+qnorm(0.95)*pod_B$a.p.sigma.z
      
      pod_B$a.p_1  =(-(b1*a1_B)-sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-pod_B$a.p.z)))/(2*b1*a2_B)
      pod_B$a.p_2  =(-(b1*a1_B)+sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-pod_B$a.p.z)))/(2*b1*a2_B)
      pod_B$a.p.q_1=(-(b1*a1_B)-sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-pod_B$a.p.q.z)))/(2*b1*a2_B)
      pod_B$a.p.q_2=(-(b1*a1_B)+sqrt(b1^2*a1_B^2-4*b1*a2_B*(b0-b1*a0_B-pod_B$a.p.q.z)))/(2*b1*a2_B)
      
      model.df=data.frame(sim_num,model.name,mat,a0_A,a1_A,a2_A,a0_B,a1_B,a2_B,b0,b1,a0_var,a0_a1_cov,a0_a2_cov,a0_a3_cov,a0_a4_cov,a0_a5_cov,a0_sigma_cov,a1_var,a1_a2_cov,a1_a3_cov,a1_a4_cov,a1_a5_cov,a1_sigma_cov,a2_var,a2_a3_cov,a2_a4_cov,a2_a5_cov,a2_sigma_cov,a3_var,a3_a4_cov,a3_a5_cov,a3_sigma_cov,a4_var,a4_a5_cov,a4_sigma_cov,a5_var,a5_sigma_cov,a.sigma_var,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,x_max,var_mult_max,sigma_max_A,sigma_max_B,varOfvar_A,varOfvar_B,mu.pod_A,mu.pod_B,sigma.pod_A,sigma.pod_B,U_1_1_A,U_1_2_A,U_2_1_A,U_2_2_A,U_3_1_A,U_3_2_A,U_1_1_B,U_1_2_B,U_2_1_B,U_2_2_B,U_3_1_B,U_3_2_B,V_1_1_A,V_1_2_A,V_1_3_A,V_2_1_A,V_2_2_A,V_2_3_A,V_3_1_A,V_3_2_A,V_3_3_A,V_1_1_B,V_1_2_B,V_1_3_B,V_2_1_B,V_2_2_B,V_2_3_B,V_3_1_B,V_3_2_B,V_3_3_B,V_POD_1_1_A,V_POD_1_2_A,V_POD_2_1_A,V_POD_2_2_A,V_POD_1_1_B,V_POD_1_2_B,V_POD_2_1_B,V_POD_2_2_B,a50.z_A,a90.z_A,a90.sigma.z_A,a90.95.z_A,a50_1_A,a50_2_A,a90_1_A,a90_2_A,a9095_1_A,a9095_2_A,a50.z_B,a90.z_B,a90.sigma.z_B,a90.95.z_B,a50_1_B,a50_2_B,a90_1_B,a90_2_B,a9095_1_B,a9095_2_B)
      model.df=c(model.df,glance(x_mod))
      
      pod=rbind(pod_A,pod_B)
      pod$a.p       = pod$a.p_2
      pod$a.p.sigma = pod$a.p.sigma.z
      pod$a.p.q     = pod$a.p.q_2
      
      # Save dataframes
      if(sim_num==1){
        write.table(model.df,file=paste(outputLocation,"model.both.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=TRUE,sep=",")
      }else{
        write.table(model.df,file=paste(outputLocation,"model.both.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      }
      write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      
    
      
    # Run x+material model___________________________________________________________
      model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
      a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
      a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
      a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
      a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
      x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
      U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
      U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
      V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
      V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
      a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
      a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
      a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
      a50=NA;a90=NA;a9095=NA;mu.pod_A=NA;mu.pod_B=NA;
      a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;
      a50_1=NA;a50_2_B=NA;a90_1_B=NA;a90_2_B=NA;a9095_1_B=NA;a9095_2_B=NA;
      
      sim_data = data
      model.df = NULL
      mat="Both"
      model.name="x+material"
      sim_data$model.name=model.name
      sim_data$mat=mat
      pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      pod_A    = data.frame(probability = seq(0,1,0.01),model.name, mat="A", a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      pod_B    = data.frame(probability = seq(0,1,0.01),model.name, mat="B", a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      
      print("x+material model_____________________")
      model.x = survreg(formula = Surv(y) ~ x+material+x*material, dist = "gaussian", data = sim_data)
      pred_dat= predict(model.x,se.fit=TRUE)
      sim_data$z  =pred_dat$fit
      sim_data$var_mult=pred_dat$se.fit^2
      
      x_mod = model.x
      x_vals = sim_data$x
      x_mod_vcov	=	vcov(x_mod)					
      
      b0=x_mod$coefficients[[1]]
      b1=x_mod$coefficients[[2]]
      b2=x_mod$coefficients[[3]]
      b3=x_mod$coefficients[[4]]
    
      a0_A=b0
      a1_A=b1
      a0_B=b0+b2
      a1_B=b1+b3
      
      b0_var		   = x_mod_vcov[[1,1]]					
      b0_b1_cov		 = x_mod_vcov[[1,2]]					
      b0_b2_cov		 = x_mod_vcov[[1,3]]					
      b0_b3_cov		 = x_mod_vcov[[1,4]]					
      b0_sigma_cov = x_mod_vcov[[1,5]]
      b1_var		   = x_mod_vcov[[2,2]]					
      b1_b2_cov		 = x_mod_vcov[[2,3]]					
      b1_b3_cov		 = x_mod_vcov[[2,4]]					
      b1_sigma_cov = x_mod_vcov[[2,5]]					
      b2_var       = x_mod_vcov[[3,3]]
      b2_b3_cov		 = x_mod_vcov[[3,4]]					
      b2_sigma_cov = x_mod_vcov[[3,5]]					
      b3_var       = x_mod_vcov[[4,4]]
      b3_sigma_cov = x_mod_vcov[[4,5]]					
      sigma_var    = x_mod_vcov[[5,5]]

      mu.pod_A=(y_dec-a0_A)/a1_A
      sigma.pod_A=x_mod$scale/a1_A
      U_A=matrix(c(-1/a1_A,-mu.pod_A/a1_A,0,0,-sigma.pod_A/a1_A,x_mod$scale/a1_A),nrow=3)
      V_A=matrix(c(b0_var,b0_b1_cov,b0_sigma_cov,b0_b1_cov,b1_var,b1_sigma_cov,b0_sigma_cov,b1_sigma_cov,sigma_var),nrow=3)
      V_POD_A = t(U_A)%*%V_A%*%U_A
      U_1_1_A = U_A[1,1]
      U_1_2_A = U_A[1,2]
      U_2_1_A = U_A[2,1]
      U_2_2_A = U_A[2,2]
      U_3_1_A = U_A[3,1]
      U_3_2_A = U_A[3,2]
      V_1_1_A = V_A[1,1]
      V_1_2_A = V_A[1,2]
      V_1_3_A = V_A[1,3]
      V_2_1_A = V_A[2,1]
      V_2_2_A = V_A[2,2]
      V_2_3_A = V_A[2,3]
      V_3_1_A = V_A[3,1]
      V_3_2_A = V_A[3,2]
      V_3_3_A = V_A[3,3]
      V_POD_1_1_A = V_POD_A[1,1]
      V_POD_1_2_A = V_POD_A[1,2]
      V_POD_2_1_A = V_POD_A[2,1]
      V_POD_2_2_A = V_POD_A[2,2]
      a50_A=qnorm(0.5)*sigma.pod_A+mu.pod_A
      a90_A=qnorm(0.9)*sigma.pod_A+mu.pod_A
      a90.sigma_A=sqrt(V_POD_1_1_A+2*qnorm(0.9)*V_POD_1_2_A+qnorm(0.9)^2*V_POD_2_2_A)
      a90.95_A=a90_A+qnorm(0.95)*a90.sigma_A
      
      mu.pod_B=(y_dec-a0_B)/a1_B
      sigma.pod_B=x_mod$scale/a1_B
      U_B=matrix(c(-1/a1_B,-mu.pod_B/a1_B,0,0,-sigma.pod_B/a1_B,x_mod$scale/a1_B),nrow=3)
      V_B=matrix(c(b0_var+b2_var+2*b0_b2_cov,b0_b1_cov+b0_b3_cov+b1_b2_cov+b2_b3_cov,b0_sigma_cov+b2_sigma_cov,b0_b1_cov+b0_b3_cov+b1_b2_cov+b2_b3_cov,b1_var+b3_var+2*b1_b3_cov,b1_sigma_cov+b3_sigma_cov,b0_sigma_cov+b2_sigma_cov,b1_sigma_cov+b3_sigma_cov,sigma_var),nrow=3)
      V_POD_B = t(U_B)%*%V_B%*%U_B
      U_1_1_B = U_B[1,1]
      U_1_2_B = U_B[1,2]
      U_2_1_B = U_B[2,1]
      U_2_2_B = U_B[2,2]
      U_3_1_B = U_B[3,1]
      U_3_2_B = U_B[3,2]
      V_1_1_B = V_B[1,1]
      V_1_2_B = V_B[1,2]
      V_1_3_B = V_B[1,3]
      V_2_1_B = V_B[2,1]
      V_2_2_B = V_B[2,2]
      V_2_3_B = V_B[2,3]
      V_3_1_B = V_B[3,1]
      V_3_2_B = V_B[3,2]
      V_3_3_B = V_B[3,3]
      V_POD_1_1_B = V_POD_B[1,1]
      V_POD_1_2_B = V_POD_B[1,2]
      V_POD_2_1_B = V_POD_B[2,1]
      V_POD_2_2_B = V_POD_B[2,2]
      a50_B=qnorm(0.5)*sigma.pod_B+mu.pod_B
      a90_B=qnorm(0.9)*sigma.pod_B+mu.pod_B
      a90.sigma_B=sqrt(V_POD_1_1_B+2*qnorm(0.9)*V_POD_1_2_B+qnorm(0.9)^2*V_POD_2_2_B)
      a90.95_B=a90_B+qnorm(0.95)*a90.sigma_B
      
      pod_A$a.p       =qnorm(pod$probability)*sigma.pod_A+mu.pod_A
      pod_A$a.p.sigma =sqrt(V_POD_1_1_A+2*qnorm(pod$probability)*V_POD_1_2_A+qnorm(pod$probability)^2*V_POD_2_2_A)
      pod_A$a.p.q     =pod_A$a.p+qnorm(0.95)*pod_A$a.p.sigma
    
      pod_B$a.p       =qnorm(pod$probability)*sigma.pod_B+mu.pod_B
      pod_B$a.p.sigma =sqrt(V_POD_1_1_B+2*qnorm(pod$probability)*V_POD_1_2_B+qnorm(pod$probability)^2*V_POD_2_2_B)
      pod_B$a.p.q     =pod_B$a.p+qnorm(0.95)*pod_B$a.p.sigma
      
      a50_1_A=a50_A;a50_1_B=a50_B;a90_1_A=a90_A;a90_1_B=a90_B;a9095_1_A=a90.95_A;a9095_1_B=a90.95_B;
      model.df=data.frame(sim_num,model.name,mat,a0_A,a1_A,a2_A,a0_B,a1_B,a2_B,b0,b1,a0_var,a0_a1_cov,a0_a2_cov,a0_a3_cov,a0_a4_cov,a0_a5_cov,a0_sigma_cov,a1_var,a1_a2_cov,a1_a3_cov,a1_a4_cov,a1_a5_cov,a1_sigma_cov,a2_var,a2_a3_cov,a2_a4_cov,a2_a5_cov,a2_sigma_cov,a3_var,a3_a4_cov,a3_a5_cov,a3_sigma_cov,a4_var,a4_a5_cov,a4_sigma_cov,a5_var,a5_sigma_cov,a.sigma_var,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,x_max,var_mult_max,sigma_max_A,sigma_max_B,varOfvar_A,varOfvar_B,mu.pod_A,mu.pod_B,sigma.pod_A,sigma.pod_B,U_1_1_A,U_1_2_A,U_2_1_A,U_2_2_A,U_3_1_A,U_3_2_A,U_1_1_B,U_1_2_B,U_2_1_B,U_2_2_B,U_3_1_B,U_3_2_B,V_1_1_A,V_1_2_A,V_1_3_A,V_2_1_A,V_2_2_A,V_2_3_A,V_3_1_A,V_3_2_A,V_3_3_A,V_1_1_B,V_1_2_B,V_1_3_B,V_2_1_B,V_2_2_B,V_2_3_B,V_3_1_B,V_3_2_B,V_3_3_B,V_POD_1_1_A,V_POD_1_2_A,V_POD_2_1_A,V_POD_2_2_A,V_POD_1_1_B,V_POD_1_2_B,V_POD_2_1_B,V_POD_2_2_B,a50.z_A,a90.z_A,a90.sigma.z_A,a90.95.z_A,a50_1_A,a50_2_A,a90_1_A,a90_2_A,a9095_1_A,a9095_2_A,a50.z_B,a90.z_B,a90.sigma.z_B,a90.95.z_B,a50_1_B,a50_2_B,a90_1_B,a90_2_B,a9095_1_B,a9095_2_B)
      model.df=c(model.df,glance(x_mod))
      
      pod=rbind(pod_A,pod_B)
      
      write.table(model.df,file=paste(outputLocation,"model.both.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      

    # Run collapsed x model___________________________________________________________
      model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
      a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
      a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
      a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
      a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
      x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
      U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
      U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
      V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
      V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
      a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
      a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
      a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
      a50=NA;a90=NA;a9095=NA;
      
      sim_data = data
      model.df = NULL
      mat="Both"
      model.name="collapsed.x"
      sim_data$model.name=model.name
      sim_data$mat=mat
      pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                            a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                            sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
      
      print("Collapsed x model_____________________")
      model.x.collapsed = survreg(formula = Surv(y) ~ x, dist = "gaussian", data = sim_data)
      summary(model.x.collapsed)
      vcov.x.collapsed = vcov(model.x.collapsed)
      pred_dat= predict(model.x.collapsed,se.fit=TRUE)
      sim_data$z  =pred_dat$fit
      sim_data$var_mult=pred_dat$se.fit^2
    
      x_mod = model.x.collapsed
      x_vals      = sim_data$x
      x_mod_vcov	=	vcov(x_mod)					
      
      b0=x_mod$coefficients[[1]]
      b1=x_mod$coefficients[[2]]
      
      b0_var		   = x_mod_vcov[[1,1]]					
      
      b1_var		   = x_mod_vcov[[2,2]]					
      b.sigma_var	 = x_mod_vcov[[3,3]]					
      b0_b1_cov		 = x_mod_vcov[[1,2]]					
      b0_sigma_cov = x_mod_vcov[[1,3]]					
      b1_sigma_cov = x_mod_vcov[[2,3]]					
      
      mu.pod=(y_dec-b0)/b1
      sigma.pod=x_mod$scale/b1
      U=matrix(c(-1/b1,-mu.pod/b1,0,0,-sigma.pod/b1,x_mod$scale/b1),nrow=3)
      V=vcov(x_mod)
      V_POD = t(U)%*%V%*%U
      
      U_1_1 = U[1,1]
      U_1_2 = U[1,2]
      U_2_1 = U[2,1]
      U_2_2 = U[2,2]
      U_3_1 = U[3,1]
      U_3_2 = U[3,2]
      
      V_1_1 = V[1,1]
      V_1_2 = V[1,2]
      V_1_3 = V[1,3]
      V_2_1 = V[2,1]
      V_2_2 = V[2,2]
      V_2_3 = V[2,3]
      V_3_1 = V[3,1]
      V_3_2 = V[3,2]
      V_3_3 = V[3,3]
      
      V_POD_1_1 = V_POD[1,1]
      V_POD_1_2 = V_POD[1,2]
      V_POD_2_1 = V_POD[2,1]
      V_POD_2_2 = V_POD[2,2]
    
      a50=qnorm(0.5)*sigma.pod+mu.pod
      a90=qnorm(0.9)*sigma.pod+mu.pod
      a90.sigma=sqrt(V_POD_1_1+2*qnorm(0.9)*V_POD_1_2+qnorm(0.9)^2*V_POD_2_2)
      a9095=a90+qnorm(0.95)*a90.sigma
      
      pod$a.p         = qnorm(pod$probability)*sigma.pod+mu.pod
      pod$a.p.sigma.z = sqrt(V_POD_1_1+2*qnorm(pod$probability)*V_POD_1_2+qnorm(pod$probability)^2*V_POD_2_2)
      pod$a.p.q       = pod$a.p+qnorm(0.95)*pod$a.p.sigma.z
      
      model.df=data.frame(sim_num,model.name,mat,NA,NA,NA,b0,b1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,NA,NA,NA,NA,mu.pod,sigma.pod,U_1_1,U_1_2,U_2_1,U_2_2,U_3_1,U_3_2,V_1_1,V_1_2,V_1_3,V_2_1,V_2_2,V_2_3,V_3_1,V_3_2,V_3_3,V_POD_1_1,V_POD_1_2,V_POD_2_1,V_POD_2_2,NA,NA,NA,NA,a50,NA,a90,NA,a9095,NA)
      model.df=c(model.df,glance(x_mod))
      
      # Save dataframes
        write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      
      
    # Run x model for A___________________________________________________________
        model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
        a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
        a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
        a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
        a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
        x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
        U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
        U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
        V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
        V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
        a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
        a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
        a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
        a50=NA;a90=NA;a9095=NA;
        
        sim_data = data_A
        model.df = NULL
        mat="A"
        model.name="x for A"
        sim_data$model.name=model.name
        sim_data$mat=mat
        pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                              a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                              sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
        
        print("x for material A model_____________________")
        model.x.collapsed.A = survreg(formula = Surv(y) ~ x, dist = "gaussian", data = sim_data)
        vcov.x.collapsed = vcov(model.x.collapsed.A)
        pred_dat= predict(model.x.collapsed.A,se.fit=TRUE)
        sim_data$z  =pred_dat$fit
        sim_data$var_mult=pred_dat$se.fit^2
        
        x_mod = model.x.collapsed.A
        x_vals      = sim_data$x
        x_mod_vcov	=	vcov(x_mod)					
        
        b0=x_mod$coefficients[[1]]
        b1=x_mod$coefficients[[2]]
        
        b0_var		   = x_mod_vcov[[1,1]]					
        b1_var		   = x_mod_vcov[[2,2]]					
        b.sigma_var	 = x_mod_vcov[[3,3]]					
        b0_b1_cov		 = x_mod_vcov[[1,2]]					
        b0_sigma_cov = x_mod_vcov[[1,3]]					
        b1_sigma_cov = x_mod_vcov[[2,3]]					
        
        mu.pod=(y_dec-b0)/b1
        sigma.pod=x_mod$scale/b1
        U=matrix(c(-1/b1,-mu.pod/b1,0,0,-sigma.pod/b1,x_mod$scale/b1),nrow=3)
        V=vcov(x_mod)
        V_POD = t(U)%*%V%*%U
        
        U_1_1 = U[1,1]
        U_1_2 = U[1,2]
        U_2_1 = U[2,1]
        U_2_2 = U[2,2]
        U_3_1 = U[3,1]
        U_3_2 = U[3,2]
        
        V_1_1 = V[1,1]
        V_1_2 = V[1,2]
        V_1_3 = V[1,3]
        V_2_1 = V[2,1]
        V_2_2 = V[2,2]
        V_2_3 = V[2,3]
        V_3_1 = V[3,1]
        V_3_2 = V[3,2]
        V_3_3 = V[3,3]
        
        V_POD_1_1 = V_POD[1,1]
        V_POD_1_2 = V_POD[1,2]
        V_POD_2_1 = V_POD[2,1]
        V_POD_2_2 = V_POD[2,2]
        
        a50=qnorm(0.5)*sigma.pod+mu.pod
        a90=qnorm(0.9)*sigma.pod+mu.pod
        a90.sigma=sqrt(V_POD_1_1+2*qnorm(0.9)*V_POD_1_2+qnorm(0.9)^2*V_POD_2_2)
        a9095=a90+qnorm(0.95)*a90.sigma
        
        pod$a.p         = qnorm(pod$probability)*sigma.pod+mu.pod
        pod$a.p.sigma.z = sqrt(V_POD_1_1+2*qnorm(pod$probability)*V_POD_1_2+qnorm(pod$probability)^2*V_POD_2_2)
        pod$a.p.q       = pod$a.p+qnorm(0.95)*pod$a.p.sigma.z
        
        model.df=data.frame(sim_num,model.name,mat,NA,NA,NA,b0,b1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,NA,NA,NA,NA,mu.pod,sigma.pod,U_1_1,U_1_2,U_2_1,U_2_2,U_3_1,U_3_2,V_1_1,V_1_2,V_1_3,V_2_1,V_2_2,V_2_3,V_3_1,V_3_2,V_3_3,V_POD_1_1,V_POD_1_2,V_POD_2_1,V_POD_2_2,NA,NA,NA,NA,a50,NA,a90,NA,a9095,NA)
        model.df=c(model.df,glance(x_mod))
        
        # Save dataframes
        write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        
    # Run x model for B___________________________________________________________
        model.name=NA;mat=NA;a0_A=NA;a1_A=NA;a2_A=NA;a0_B=NA;a1_B=NA;a2_B=NA;b0=NA;b1=NA;
        a0_var=NA;a0_a1_cov=NA;a0_a2_cov=NA;a0_a3_cov=NA;a0_a4_cov=NA;a0_a5_cov=NA;a0_sigma_cov=NA;a1_var=NA;
        a1_a2_cov=NA;a1_a3_cov=NA;a1_a4_cov=NA;a1_a5_cov=NA;a1_sigma_cov=NA;a2_var=NA;a2_a3_cov=NA;a2_a4_cov=NA;a2_a5_cov=NA;
        a2_sigma_cov=NA;a3_var=NA;a3_a4_cov=NA;a3_a5_cov=NA;a3_sigma_cov=NA;a4_var=NA;a4_a5_cov=NA;a4_sigma_cov=NA;a5_var=NA;
        a5_sigma_cov=NA;a.sigma_var=NA;b0_var=NA;b1_var=NA;b.sigma_var=NA;b0_b1_cov=NA;b0_sigma_cov=NA;b1_sigma_cov=NA;
        x_max=NA;var_mult_max=NA;sigma_max_A=NA;sigma_max_B=NA;varOfvar_A=NA;varOfvar_B=NA;mu.pod=NA;sigma.pod_A=NA;sigma.pod_B=NA;
        U_1_1_A=NA;U_1_2_A=NA;U_2_1_A=NA;U_2_2_A=NA;U_3_1_A=NA;U_3_2_A=NA;U_1_1_B=NA;U_1_2_B=NA;U_2_1_B=NA;U_2_2_B=NA;U_3_1_B=NA;
        U_3_2_B=NA;V_1_1_A=NA;V_1_2_A=NA;V_1_3_A=NA;V_2_1_A=NA;V_2_2_A=NA;V_2_3_A=NA;V_3_1_A=NA;V_3_2_A=NA;V_3_3_A=NA;V_1_1_B=NA;
        V_1_2_B=NA;V_1_3_B=NA;V_2_1_B=NA;V_2_2_B=NA;V_2_3_B=NA;V_3_1_B=NA;V_3_2_B=NA;V_3_3_B=NA;V_POD_1_1_A=NA;V_POD_1_2_A=NA;
        V_POD_2_1_A=NA;V_POD_2_2_A=NA;V_POD_1_1_B=NA;V_POD_1_2_B=NA;V_POD_2_1_B=NA;V_POD_2_2_B=NA;a50.z_A=NA;a90.z_A=NA;
        a90.sigma.z_A=NA;a90.95.z_A=NA;a50_1_A=NA;a50_2_A=NA;a90_1_A=NA;a90_2_A=NA;a9095_1_A=NA;a9095_2_A=NA;a50.z_B=NA;
        a90.z_B=NA;a90.sigma.z_B=NA;a90.95.z_B=NA;a50_B=NA;a50_B=NA;a90_B=NA;a90_B=NA;a9095_B=NA;a9095_B=NA;
        a50.z=NA;a90.z=NA;a90.sigma.z=NA;a90.95.z=NA;a50_1=NA;a50_2=NA;a90_1=NA;a90_2=NA;a9095_1=NA;a9095_2=NA;
        a50=NA;a90=NA;a9095=NA;

        sim_data = data_B
        model.df = NULL
        mat="B"
        model.name="x for B"
        sim_data$model.name=model.name
        sim_data$mat=mat
        pod      = data.frame(probability = seq(0,1,0.01),model.name, mat, a.p.z=NA, a.p.sigma.z=NA,
                              a.p.q.z=NA,	a.p_1=NA,	a.p_2=NA,	a.p.q_1=NA,	a.p.q_2=NA,
                              sim_num, a.p=NA,  a.p.sigma=NA, a.p.q=NA)
        
        print("x for material B model_____________________")
        model.x.collapsed.B = survreg(formula = Surv(y) ~ x, dist = "gaussian", data = sim_data)
        summary(model.x.collapsed.B)
        vcov.x.collapsed = vcov(model.x.collapsed.B)
        pred_dat= predict(model.x.collapsed.B,se.fit=TRUE)
        sim_data$z  =pred_dat$fit
        sim_data$var_mult=pred_dat$se.fit^2
        
        x_mod = model.x.collapsed.B
        x_vals      = sim_data$x
        x_mod_vcov	=	vcov(x_mod)					
        
        b0=x_mod$coefficients[[1]]
        b1=x_mod$coefficients[[2]]
        
        b0_var		   = x_mod_vcov[[1,1]]					
        b1_var		   = x_mod_vcov[[2,2]]					
        b.sigma_var	 = x_mod_vcov[[3,3]]					
        b0_b1_cov		 = x_mod_vcov[[1,2]]					
        b0_sigma_cov = x_mod_vcov[[1,3]]					
        b1_sigma_cov = x_mod_vcov[[2,3]]					
        
        mu.pod=(y_dec-b0)/b1
        sigma.pod=x_mod$scale/b1
        U=matrix(c(-1/b1,-mu.pod/b1,0,0,-sigma.pod/b1,x_mod$scale/b1),nrow=3)
        V=vcov(x_mod)
        V_POD = t(U)%*%V%*%U
        
        U_1_1 = U[1,1]
        U_1_2 = U[1,2]
        U_2_1 = U[2,1]
        U_2_2 = U[2,2]
        U_3_1 = U[3,1]
        U_3_2 = U[3,2]
        
        V_1_1 = V[1,1]
        V_1_2 = V[1,2]
        V_1_3 = V[1,3]
        V_2_1 = V[2,1]
        V_2_2 = V[2,2]
        V_2_3 = V[2,3]
        V_3_1 = V[3,1]
        V_3_2 = V[3,2]
        V_3_3 = V[3,3]
        
        V_POD_1_1 = V_POD[1,1]
        V_POD_1_2 = V_POD[1,2]
        V_POD_2_1 = V_POD[2,1]
        V_POD_2_2 = V_POD[2,2]
        
        a50=qnorm(0.5)*sigma.pod+mu.pod
        a90=qnorm(0.9)*sigma.pod+mu.pod
        a90.sigma=sqrt(V_POD_1_1+2*qnorm(0.9)*V_POD_1_2+qnorm(0.9)^2*V_POD_2_2)
        a9095=a90+qnorm(0.95)*a90.sigma
        
        pod$a.p         = qnorm(pod$probability)*sigma.pod+mu.pod
        pod$a.p.sigma.z = sqrt(V_POD_1_1+2*qnorm(pod$probability)*V_POD_1_2+qnorm(pod$probability)^2*V_POD_2_2)
        pod$a.p.q       = pod$a.p+qnorm(0.95)*pod$a.p.sigma.z
        
        model.df=data.frame(sim_num,model.name,mat,NA,NA,NA,b0,b1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,b0_var,b1_var,b.sigma_var,b0_b1_cov,b0_sigma_cov,b1_sigma_cov,NA,NA,NA,NA,mu.pod,sigma.pod,U_1_1,U_1_2,U_2_1,U_2_2,U_3_1,U_3_2,V_1_1,V_1_2,V_1_3,V_2_1,V_2_2,V_2_3,V_3_1,V_3_2,V_3_3,V_POD_1_1,V_POD_1_2,V_POD_2_1,V_POD_2_2,NA,NA,NA,NA,a50,NA,a90,NA,a9095,NA)
        model.df=c(model.df,glance(x_mod))
        
        # Save dataframes
        write.table(sim_data,file=paste(outputLocation,"sim_data.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        write.table(pod,file=paste(outputLocation,"pod.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        write.table(model.df,file=paste(outputLocation,"model.df.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        
        
# DO ALL THE ANOVA TESTS AND SAVE THEM OUT    
        print("LRT_____________________")
        anova_results=NULL
        anova_results=rbind(anova(model.x.collapsed,model.x),anova(model.collapsed.x2,model.x2),
                            anova(model.x.collapsed,model.x),anova(model.x,model.x2),
                            anova(model.x.collapsed,model.collapsed.x2),anova(model.x,model.x2),
                            anova(model.x.collapsed.A,model.x.collapsed2A),anova(model.x.collapsed.B,model.x.collapsed2B))
        anova_results$sim_num=sim_num
        
        if(sim_num==1){
        write.table(anova_results,file=paste(outputLocation,"LRT.csv",sep=""),append=TRUE,row.names=FALSE,col.names=TRUE,sep=",")
        }else{
          write.table(anova_results,file=paste(outputLocation,"LRT.csv",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
        }
                
}        
        