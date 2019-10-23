#Notes

#Independent Variable 
#R01 (Gender): (1) Male, (2) Females
#R18 (Social Class): (1) Middle Class, (2) Working Class 

#Dependent Variable 
#A07 (Party identification): (1) Strong Democrat (2) Weak Democrat (3) Independent Democrat (4) Independent (5) Independent Republican (6) Weak Republican (7) Strong Republican
#G01 (Ideology): (1) Very liberal (2) Liberal (3) Slightly liberal (4) Moderate (5) Slightly conservative (6) Conservative (7) Very conservative
#G04 (Democratic Party: Ideological placement): (1) Very liberal (2) Liberal (3) Slightly liberal (4) Moderate (5) Slightly conservative (6) Conservative (7) Very conservative
#G05 (Republican Party: Ideological placement): (1) Very liberal (2) Liberal (3) Slightly liberal (4) Moderate (5) Slightly conservative (6) Conservative (7) Very conservative
#G06 (Dem Party: Left-right placement): (1) Left (2) Somewhat left (3) Middle of the road (4) Somewhat right (5) Right
#G07 (Rep Party: Left-right placement): (1) Left (2) Somewhat left (3) Middle of the road (4) Somewhat right (5) Right

# Setup Data
setwd("C:/Users/dontran/Desktop/ICPSR_34808/DS0001") 
mydata = load("34808-0001-Data.rda")
head(mydata)
head(da34808.0001)
mydata = da34808.0001
head(mydata) 

# Gender
R01A07 = mydata[, c("R01", "A07")] 
R01G01 = mydata[, c("R01", "G01")]
R01G04 = mydata[, c("R01", "G04")]
R01G05 = mydata[, c("R01", "G05")]
R01G06 = mydata[, c("R01", "G06")]
R01G07 = mydata[, c("R01", "G07")] 

# Social Class 
R18A07 = mydata[, c("R18", "A07")] 
R18G01 = mydata[, c("R18", "G01")]
R18G04 = mydata[, c("R18", "G04")]
R18G05 = mydata[, c("R18", "G05")]
R18G06 = mydata[, c("R18", "G06")]
R18G07 = mydata[, c("R18", "G07")]

# Subset Data For Gender 
SR01A07 = R01A07[R01A07$R01 %in% c("(1) Male", "(2) Female"), ]
SR01G01 = R01G01[R01G01$R01 %in% c("(1) Male", "(2) Female"), ] 
SR01G04 = R01G04[R01G04$R01 %in% c("(1) Male", "(2) Female"), ] 
SR01G05 = R01G05[R01G05$R01 %in% c("(1) Male", "(2) Female"), ] 
SR01G06 = R01G06[R01G06$R01 %in% c("(1) Male", "(2) Female"), ] 
SR01G07 = R01G07[R01G07$R01 %in% c("(1) Male", "(2) Female"), ]

# Subset Data for Social Class 
SR18A07 = R18A07[R18A07$R18 %in% c("(1) Middle class", "(2) Working class"), ] 
SR18G01 = R18G01[R18G01$R18 %in% c("(1) Middle class", "(2) Working class"), ]
SR18G04 = R18G04[R18G04$R18 %in% c("(1) Middle class", "(2) Working class"), ]
SR18G05 = R18G05[R18G05$R18 %in% c("(1) Middle class", "(2) Working class"), ]
SR18G06 = R18G06[R18G06$R18 %in% c("(1) Middle class", "(2) Working class"), ]
SR18G07 = R18G07[R18G07$R18 %in% c("(1) Middle class", "(2) Working class"), ]

# Convert for gender  
cgA07 = as.numeric(SR01A07$A07)
#is.numeric(cgA07)
cgG01 = as.numeric(SR01G01$G01)
#is.numeric(cgG01)
cgG04 = as.numeric(SR01G04$G04)
#is.numeric(cgG04)
cgG05 = as.numeric(SR01G05$G05)
#is.numeric(cgG05)
cgG06 = as.numeric(SR01G06$G06) 
#is.numeric(cgG06)
cgG07 = as.numeric(SR01G07$G07)
#is.numeric(cgG07) 

# Convert for Social class 
csA07 = as.numeric(SR18A07$A07) 
#is.numeric(csA01)
csG01 = as.numeric(SR18G01$G01) 
#is.numeric(csG01)
csG04 = as.numeric(SR18G04$G04)
#is.numeric(csG04)
csG05 = as.numeric(SR18G05$G05) 
#is.numeric(csG05)
csG06 = as.numeric(SR18G06$G06)
#is.numeric(csG06)
csG07 = as.numeric(SR18G07$G07)
#is.numeric(csG07) 

# T-test 
# Gender 
ttest1 = t.test(cgA07~SR01A07$R01, var.equal = T, na.action="na.exclude") 
ttest2 = t.test(cgG01~SR01G01$R01, var.equal = T, na.action="na.exclude")
ttest3 = t.test(cgG04~SR01G04$R01, var.equal = T, na.action="na.exclude")
ttest4 = t.test(cgG05~SR01G05$R01, var.equal = T, na.action="na.exclude")
ttest5 = t.test(cgG06~SR01G06$R01, var.equal = T, na.action="na.exclude")
ttest6 = t.test(cgG07~SR01G07$R01, var.equal = T, na.action="na.exclude")

# Social Class 
ttest11 = t.test(csA07~SR18A07$R18, var.equal = T, na.action="na.exclude") 
ttest12 = t.test(csG01~SR18G01$R18, var.equal = T, na.action="na.exclude")
ttest13 = t.test(csG04~SR18G01$R18, var.equal = T, na.action="na.exclude")
ttest14 = t.test(csG05~SR18G05$R18, var.equal = T, na.action="na.exclude")
ttest15 = t.test(csG06~SR18G06$R18, var.equal = T, na.action="na.exclude")
ttest16 = t.test(csG07~SR18G07$R18, var.equal = T, na.action="na.exclude")

# Statstics

# Conversion for Statistics and graphs 
R01i = as.integer(mydata$R01)
R18i = as.integer(mydata$R18) 
G01i = as.integer(mydata$G01)
G04i = as.integer(mydata$G04)
G05i = as.integer(mydata$G05)
G06i = as.integer(mydata$G06)
G07i = as.integer(mydata$G07)
A07i = as.integer(mydata$A07)

#Specfic data for IV(WIP)  

# Mean 
aR01 = mean(R01i, na.rm = T)
aR18 = mean(R18i, na.rm = T)
aG01 = mean(G01i, na.rm = T)
aG04 = mean(G04i, na.rm = T)
aG05 = mean(G05i, na.rm = T)
aG06 = mean(G06i, na.rm = T)
aG07 = mean(G07i, na.rm = T)
aA07 = mean(A07i, na.rm = T)

# Median 
bR01 = median(R01i, na.rm = T)
bR18 = median(R18i, na.rm = T)
bG01 = median(G01i, na.rm = T)
bG04 = median(G04i, na.rm = T)
bG05 = median(G05i, na.rm = T)
bG06 = median(G06i, na.rm = T)
bG07 = median(G07i, na.rm = T) 
bA07 = median(A07i, na.rm = T)

# Mode / raw vaules
cR01 = table(R01i)
cR18 = table(R18i)
cG01 = table(G01i)
cG04 = table(G04i)
cG05 = table(G05i)
cG06 = table(G06i)
cG07 = table(G07i)
cA07 = table(A07i)

# Variance 
dR01 = var(R01i, na.rm = T)
dR18 = var(R18i, na.rm = T)
dG01 = var(G01i, na.rm = T)
dG04 = var(G04i, na.rm = T)
dG05 = var(G05i, na.rm = T)
dG06 = var(G06i, na.rm = T)
dG07 = var(G07i, na.rm = T) 
dA07 = var(A07i, na.rm = T)

#standard Deviation 
eR01 = sd(R01i, na.rm = T)
eR18 = sd(R18i, na.rm = T)
eG01 = sd(G01i, na.rm = T)
eG04 = sd(G04i, na.rm = T)
eG05 = sd(G05i, na.rm = T)
eG06 = sd(G06i, na.rm = T)
eG07 = sd(G07i, na.rm = T)
eA07 = sd(A07i, na.rm = T)

#Graphs  

#Pie graphs 
fR01 = pie(table(mydata$R01), main = "R01")
fR18 = pie(table(mydata$R18), main = "R18")
fG01 = pie(table(mydata$G01), main = "G01")
fG04 = pie(table(mydata$G04), main = "G04")
fG05 = pie(table(mydata$G05), main = "G05")
fG06 = pie(table(mydata$G06), main = "G06")
fG07 = pie(table(mydata$G07), main = "G07") 

#Histograms
gR01 = hist(R01i, breaks = 2)
gR18 = hist(R18i, breaks = 2)
gG01 = hist(G01i, breaks = 5)
gG04 = hist(G04i, breaks = 5)
gG05 = hist(G05i, breaks = 5)
gG06 = hist(G06i, breaks = 5)
gG07 = hist(G07i, breaks = 5)

