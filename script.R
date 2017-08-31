Safety.Stock.Example.Data <- read.csv("Safety-Stock-Example-Data.csv", stringsAsFactors=FALSE)
Safety.Stock.Assignment <- read.csv("Safety-Stock-Assignment.csv", stringsAsFactors=FALSE)
SafetyStockAssignment <- data.frame(c(rep('A',30),rep('B',30),rep('C',30),rep('D',30),rep('E',30)),
                                    as.numeric(as.character(c(Safety.Stock.Assignment$Product.A[-1],Safety.Stock.Assignment$Product.B[-1],Safety.Stock.Assignment$Product.C[-1],Safety.Stock.Assignment$Product.D[-1],Safety.Stock.Assignment$Product.E[-1]))),
                                    as.numeric(as.character(c(Safety.Stock.Assignment$X[-1],Safety.Stock.Assignment$X.1[-1],Safety.Stock.Assignment$X.2[-1],Safety.Stock.Assignment$X.3[-1],Safety.Stock.Assignment$X.4[-1]))))
colnames(SafetyStockAssignment) <-c("Product","Demand","LeadTime")
inventoryquizp1 <- read.csv("inventoryquizp1.csv", sep=";")
inventoryquizp1$D <- as.numeric(inventoryquizp1$D)
inventoryquizp1$V <- as.numeric(gsub('\\$','',inventoryquizp1$V))
inventoryquizp1$O <- as.numeric(gsub('\\$','',inventoryquizp1$O))
inventoryquizp1$C<- as.numeric(gsub('\\%','',inventoryquizp1$C))/100


InventoryCarryingCost <- function(value, carryingcost, quantity){
 value * carryingcost * quantity / 2;  
}

OrderReceivingCost <- function(ordercost, demand, quantity)
{
  ordercost * demand / quantity
}

TotalCost <- function(value, carryingcost, ordercost, demand, quantity)
{
  InventoryCarryingCost(value, carryingcost, quantity) + OrderReceivingCost(ordercost, demand, quantity)
}

EconomicOrderQuantity <- function(value, carryingcost, ordercost, demand)
{
  sqrt(2 * ordercost * demand / (carryingcost * value)) 
}

Sc <- function(t,d)
{
  sqrt((mean(t,na.rm=TRUE) * sd(d, na.rm = TRUE)^2) + (mean(d,na.rm=TRUE)^2 * sd(t, na.rm = TRUE)^2 ))
}

k <- function(sl)
{
  qnorm(sl)
}

SS <- function(Sc,sl)
{
  Sc * k(sl)
}

leadTimeDemand <- function(d, t){mean(d,na.rm=TRUE) * mean(t,na.rm=TRUE)}


inventoryquizp1$EOQ <- EconomicOrderQuantity(inventoryquizp1$V,inventoryquizp1$C,inventoryquizp1$O,inventoryquizp1$D)
inventoryquizp1$IC <- apply(X = inventoryquizp1,MARGIN = 1,FUN = function(x){InventoryCarryingCost(as.numeric(x[3]),as.numeric(x[5]),as.numeric(x[6]))})
inventoryquizp1$OC <- OrderReceivingCost(ordercost = inventoryquizp1$O,demand = inventoryquizp1$D ,quantity = inventoryquizp1$EOQ)
inventoryquizp1$TC <- inventoryquizp1$IC + inventoryquizp1$OC
ScA <- Sc(SafetyStockAssignment[SafetyStockAssignment$Product =="A",]$LeadTime,SafetyStockAssignment[SafetyStockAssignment$Product =="A",]$Demand)
ScB <- Sc(SafetyStockAssignment[SafetyStockAssignment$Product =="B",]$LeadTime,SafetyStockAssignment[SafetyStockAssignment$Product =="B",]$Demand)
ScC <- Sc(SafetyStockAssignment[SafetyStockAssignment$Product =="C",]$LeadTime,SafetyStockAssignment[SafetyStockAssignment$Product =="C",]$Demand)
ScD <- Sc(SafetyStockAssignment[SafetyStockAssignment$Product =="D",]$LeadTime,SafetyStockAssignment[SafetyStockAssignment$Product =="D",]$Demand)
ScE <- Sc(SafetyStockAssignment[SafetyStockAssignment$Product =="E",]$LeadTime,SafetyStockAssignment[SafetyStockAssignment$Product =="E",]$Demand)
LTDA <- leadTimeDemand(SafetyStockAssignment[SafetyStockAssignment$Product =="A",]$Demand,SafetyStockAssignment[SafetyStockAssignment$Product =="A",]$LeadTime)
LTDB <- leadTimeDemand(SafetyStockAssignment[SafetyStockAssignment$Product =="B",]$Demand,SafetyStockAssignment[SafetyStockAssignment$Product =="B",]$LeadTime)
LTDC <- leadTimeDemand(SafetyStockAssignment[SafetyStockAssignment$Product =="C",]$Demand,SafetyStockAssignment[SafetyStockAssignment$Product =="C",]$LeadTime)
LTDD <- leadTimeDemand(SafetyStockAssignment[SafetyStockAssignment$Product =="D",]$Demand,SafetyStockAssignment[SafetyStockAssignment$Product =="D",]$LeadTime)
LTDE <- leadTimeDemand(SafetyStockAssignment[SafetyStockAssignment$Product =="E",]$Demand,SafetyStockAssignment[SafetyStockAssignment$Product =="E",]$LeadTime)

