from gurobipy import *

try:

    m = Model ("test1")
    
    # The floating point variables
    x1 = m.addVar (name="x1")
    x2 = m.addVar (name="x2")
    x3 = m.addVar (name="x3")
    
    # The W variables
    w1 = m.addVar(vtype=GRB.INTEGER,name="W1")
    w2 = m.addVar(vtype=GRB.INTEGER,name="W2")
    w3 = m.addVar(vtype=GRB.INTEGER,name="W3")
    
    # The TS variable
    TS = m.addVar(vtype=GRB.INTEGER,name="TS")

    # The TP variables
    TP1 = m.addVar(vtype=GRB.INTEGER,name="TP1")
    TP2 = m.addVar(vtype=GRB.INTEGER,name="TP2")
    TP3 = m.addVar(vtype=GRB.INTEGER,name="TP3")
    
    # Integrate the vriables in to the model
    m.update()
    
    # The objective function
    m.setObjective(TP1 + TP2 + TP3)
    
    # The first 3 constraints
    m.addConstr(x1 >= 0.1,"v")
    m.addConstr(x2 >= 0.1,"z")
    m.addConstr(x3 >= 0.1,"y")
    
    # The constraint list
    m.addConstr(x1 + x2 + x3 >= 1, "c3")

    m.addConstr(w1 - 3 * x1, GRB.EQUAL, 0, "c4")
    m.addConstr(w2 - 3 * x2, GRB.EQUAL, 0, "c6")
    m.addConstr(w3 - 3 * x3, GRB.EQUAL, 0, "c8")

    # TS
    m.addConstr(TS, GRB.EQUAL, w1 + w2 + w3, "c10")
    
    # TP constraints
    m.addConstr(TP1, GRB.EQUAL, 100 * TS, "c2345")
    m.addConstr(TP2, GRB.EQUAL, 50 * TS, "c122")
    m.addConstr(TP3, GRB.EQUAL, 5 * TS, "c13")
    
    # Meeting the reaction time problem
    m.addConstr(TP1 <= 240 * w1)
    m.addConstr(TP2 <= 120 * w2)
    m.addConstr(TP3 <= 40 * w3)
 
    # Finally optimize!!
    m.optimize()
    m.printStats()
    m.printQuality()
    
    print m.getObjective().getValue()
    print TP1
    print TP2
    print TP3
    print x1
    print x2
    print x3
    print w1
    print w2
    print w3
    print TS

except GurobiError:
    print "Error"
