# model equation
# Model = -1.564 + 0.141 (gender female) - 0.245 (indigeneous_yes) + 1.180 (referral type_bilateral) + 1.512 (craniofacial abnormalities_yes) 
# + 1.297 (family history_yes) + 0.993 (syndrome_yes).

# The variables are: gender (F); ATSI(Y); Referral Type (Bilat); Craniofacial (Y); Fam Hx (Y); Syndrome (Y)

# the app can have check boxes
# make 1 yes and 0 no - so if they check the box that variable will be 1

# make a dummy df to create the model
sex_f = rbinom(20, 1, 0.5)
atsi_y = rbinom(20, 1, 0.5)
ref_bilat = rbinom(20, 1, 0.5)
cranio_y = rbinom(20, 1, 0.5)
famHx_y = rbinom(20, 1, 0.5)
syndrome_y = rbinom(20, 1, 0.5)
y = rbinom(20, 1, 0.5)
intercept = rep(-2.030, 20)

df = cbind.data.frame(sex_f, atsi_y, ref_bilat, cranio_y, famHx_y, syndrome_y, y, intercept)

f <- glm(y ~ 0 + offset(intercept) + offset(0.273 * sex_f) + offset(-0.302 * atsi_y) + offset(1.429 * ref_bilat) + offset(0.985 * cranio_y) 
         + offset(1.529 * famHx_y) + offset(0.942 * syndrome_y), data = df, family = binomial(link='logit'))
f 

sex_f = 0
atsi_y = 1
ref_bilat = 0
cranio_y = 0
famHx_y = 0
syndrome_y = 0

new_data = data.frame(intercept, sex_f, atsi_y, ref_bilat, cranio_y, famHx_y, syndrome_y)

pred = predict(f, newdata = new_data, type='response')
pred

# save model
saveRDS(f, file = "model.rds") 

model = readRDS("model.rds")
summary(model)

# check model by manually making some predictions (see if they are the same as my model)
sex_f = 0
atsi_y = 1
ref_bilat = 0
cranio_y = 0
famHx_y = 0
syndrome_y = 0
manual_model = -2.030 + (0.273 * sex_f) - (0.302 * atsi_y) + (1.429 * ref_bilat) + (0.985 * cranio_y) + (1.529 * famHx_y) + (0.942 * syndrome_y)
odds = exp(manual_model)
prob = odds/(1 + odds)
prob 
