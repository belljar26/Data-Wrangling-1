# Data Wrangling Assignment
# fix "company" column names -all lower case, fix misspelling and spacing
str(refine_1_)
b<- as.factor(refine_1_$company)
levels(b)
as.character(b)
class(refine_1_$company)
refine_1_$company[refine_1_$company =="akz0"]<-"akzo"
refine_1_$company[refine_1_$company =="Phillips"] <- "phillips"
refine_1_$company[refine_1_$company == "philips"]<- "phillips"
refine_1_$company[refine_1_$company == "phllips"] <- "phillips"
refine_1_$company[refine_1_$company == "phillipS"] <- "phillips"
refine_1_$company[refine_1_$company == "Akzo"] <- "akzo"
refine_1_$company[refine_1_$company == "AKZO"]<- "akzo"
refine_1_$company[refine_1_$company == "ak zo"] <-"akzo"
refine_1_$company[refine_1_$company == "fillips"] <- "phillips"
refine_1_$company[(refine_1_$company =="unilver") |
                    (refine_1_$company =="Unilever")] <- "unilever"
refine_1_$company[(refine_1_$company == "van houten") |
                    (refine_1_$company == "van Houten") |
                    (refine_1_$company == "Van Houten")]<-"vanhouten"
#Fix Product Code & Number Column--split and create two using reshape pkg
products <- colsplit(refine_1_$`Product code / number`, "-" , 
         c("productcode", "productnumber"))
View(products)
#combine products columns with original data set renamed to refine_2
refine_2 <- cbind(refine_1_, products)
                                                           
View(refine_2)
class(refine_2)
# Delete old columnn (product/product number)
refine_2[, "Product code / number"]= NULL
View(refine_2)
# Reorder columns
refine_3 <- refine_2[c(1,6,7,2,3,4,5)]
View(refine_3)
# Add Product Category using plyr pkg
refine_3$productcat <- revalue(refine_3$productcode, 
                               c("p"= "Smartphone",
                                 "x"= "Laptop",
                                 "v" = "TV",
                                 "q"= "Tablet"))
# Reorder columns
refine_4 <- refine_3[c(1,2,3,8,4,5,6,7)]
View(refine_4)
# Add full address
refine_4$fulladdress <- paste(refine_4$address, refine_4$city,
                              refine_4$country, sep =",")
# Delete single address, city, country to clean up data frame
refine_5 <- refine_4[,-c(5:7)]
View(refine_5)
# Create columns for company with 0, 1 using dplyr
refine_6 <- refine_5 %>%
  mutate(company_phillips = ifelse(company== "phillips", 1,0)) %>%
  mutate(company_akzo = ifelse(company=="akzo", 1, 0)) %>%
  mutate(company_unilever= ifelse(company=="unilever", 1, 0))%>%
  mutate(company_vanhouten= ifelse(company=="vanhouten", 1,0))
View(refine_6)
#Create columns for product with 0,1 using dplyr pkg
refine_7 <- refine_6 %>%
  mutate(product_smartphone = ifelse(productcat== "Smartphone", 1,0))%>%
  mutate(product_laptop= ifelse(productcat== "Laptop", 1, 0))%>%
  mutate(product_tv = ifelse(productcat== "TV", 1,0)) %>%
  mutate(product_tablet= ifelse(productcat== "Tablet", 1, 0))
View(refine_7)
#save file 
write.csv(refine_7, "refine_7.csv")
