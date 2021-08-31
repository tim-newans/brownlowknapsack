library(lpSolveAPI)
{
 data <-  data.frame(Judge = rep(c(rep("A",5),rep("B",5)),7),
                     Player =c(rep("Libba",10),rep("Macrae",10),rep("Bont",10),rep("Weightman",10),
                     rep("Parish",10),rep("Naughton",10),rep("Ridley",10)),
                     Votes = rep(1:5,14),
                     Libba = c(rep(1:5,2),rep(0,60)),
                     Macrae = c(rep(0,10),rep(1:5,2),rep(0,50)),
                     Bont = c(rep(0,20),rep(1:5,2),rep(0,40)),
                     Weightman = c(rep(0,30),rep(1:5,2),rep(0,30)),
                     Parish = c(rep(0,40),rep(1:5,2),rep(0,20)),
                     Naughton = c(rep(0,50),rep(1:5,2),rep(0,10)),
                     Ridley = c(rep(0,60),rep(1:5,2)),
                     Ob = rep(0,70))

  knapsack <- make.lp(0, nrow(data))

  ## Add Brownlow Point Constraints
  add.constraint(knapsack, data$Libba, "=", 7)
  add.constraint(knapsack, data$Macrae, "=", 6)
  add.constraint(knapsack, data$Bont, "=", 5)
  add.constraint(knapsack, data$Weightman, "=", 5)
  add.constraint(knapsack, data$Parish, "=", 4)
  add.constraint(knapsack, data$Naughton, "=", 2)
  add.constraint(knapsack, data$Ridley, "=", 1)
  
  ## Add Judge constraints
  judgea1 <- ifelse(data$Judge == "A" & data$Votes == 1, 1, 0)
  judgea2 <- ifelse(data$Judge == "A" & data$Votes == 2, 1, 0)
  judgea3 <- ifelse(data$Judge == "A" & data$Votes == 3, 1, 0)
  judgea4 <- ifelse(data$Judge == "A" & data$Votes == 4, 1, 0)
  judgea5 <- ifelse(data$Judge == "A" & data$Votes == 5, 1, 0)
  judgeb1 <- ifelse(data$Judge == "B" & data$Votes == 1, 1, 0)
  judgeb2 <- ifelse(data$Judge == "B" & data$Votes == 2, 1, 0)
  judgeb3 <- ifelse(data$Judge == "B" & data$Votes == 3, 1, 0)
  judgeb4 <- ifelse(data$Judge == "B" & data$Votes == 4, 1, 0)
  judgeb5 <- ifelse(data$Judge == "B" & data$Votes == 5, 1, 0)
  add.constraint(knapsack, judgea1, "=", 1)
  add.constraint(knapsack, judgea2, "=", 1)
  add.constraint(knapsack, judgea3, "=", 1)
  add.constraint(knapsack, judgea4, "=", 1)
  add.constraint(knapsack, judgea5, "=", 1)
  add.constraint(knapsack, judgeb1, "=", 1)
  add.constraint(knapsack, judgeb2, "=", 1)
  add.constraint(knapsack, judgeb3, "=", 1)
  add.constraint(knapsack, judgeb4, "=", 1)
  add.constraint(knapsack, judgeb5, "=", 1)
  
  ## Add Player Constraints
  libba <- ifelse(data$Player == "Libba", 1, 0)
  macrae <- ifelse(data$Player == "Macrae", 1, 0)
  bont <- ifelse(data$Player == "Bont", 1, 0)
  weightman <- ifelse(data$Player == "Weightman", 1, 0)
  parish <- ifelse(data$Player == "Parish", 1, 0)
  naughton <- ifelse(data$Player == "Naughton", 1, 0)
  ridley <- ifelse(data$Player == "Ridley", 1, 0)
  add.constraint(knapsack, libba, "<=", 2)
  add.constraint(knapsack, macrae, "<=", 2)
  add.constraint(knapsack, bont, "<=", 2)
  add.constraint(knapsack, weightman, "<=", 2)
  add.constraint(knapsack, parish, "<=", 2)
  add.constraint(knapsack, naughton, "<=", 2)
  add.constraint(knapsack, ridley, "<=", 2)

  ## Make sure the decision variables are binary
  set.type(knapsack, seq(1, nrow(data), by=1), type = c("binary"))
  
  ## Solve the model, if this returns 0 an optimal solution is found
  rc<-solve(knapsack)
  sols<-list()
  obj0<-get.objective(knapsack)
  # find more solutions
  while(TRUE) {
    sol <- round(get.variables(knapsack))
    sols <- c(sols,list(sol))
    add.constraint(knapsack,2*sol-1,"<=", sum(sol)-1)
    rc<-solve(knapsack)
    if (rc!=0) break;
    if (get.objective(knapsack)<obj0-1e-6) break;
  }
  sols  
}

## Get the players on the team
team_select <- subset(as.data.frame(data, sol = sols[1]), sol == 1)
team_select$sol <- NULL
team_select 

