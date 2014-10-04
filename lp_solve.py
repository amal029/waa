from gurobipy import *
import sys

usage_message = 'gurobi.sh lp_solve.py <file-name>.txt'

if len(sys.argv) != 2:
    print usage_message
    sys.exit(1)

try:
    f = open(str(sys.argv[1]))
    d = {}
    counter = 0
    for line in f:
        lstr = line.split(',')
        lstr = map (lambda x: int(x.rstrip('\n\r')), lstr)
        d.update({counter : lstr})
        counter +=1 
    f.close()
    
    
    # Now start making the gurobi formulation
    
    # First name the model
    m = Model (sys.argv[0].strip(".")[0])
    
    # Next make the x's
    xs = map (lambda x: m.addVar(name='x'.join(str(x))), xrange (0,len(d))) 
    
    # Make the W's
    ws = map (lambda x: m.addVar(vtype=GRB.INTEGER,
                                 name='W'.join(str(x))), xrange (0,len(d))) 
    # Make the TS
    TS = m.addVar(vtype=GRB.INTEGER,name="TS")
    
    # Make the TP's
    tps = map (lambda x: m.addVar(vtype=GRB.INTEGER,
                                 name='TP'.join(str(x))), xrange (0,len(d))) 
    
    # Integrate the variables into the model
    m.update()
    
    # Set the objective function
    if len(tps) > 1:
        m.setObjective(quicksum(tps))
    else:
        m.setObjective(tps[0])
        
    # Set the xs >= 0.0 constraints
    map (lambda (i,x): m.addConstr(x >= 0.0,name='c'.join(str(i))), enumerate(xs))
    
    # Add the total constraint list
    if len(xs) > 1:
        m.addConstr(quicksum(xs) >= 1, 'add_c')
    else:
        m.addConstr(xs[0] >= 1)

    # The weight constraints
    if len(ws) != len(xs):
        print "Interal error: ws != xs"
        print ws
        print xs
        sys.exit(1)
    else:
        map(lambda x,w: m.addConstr(-len(xs) * x + w,
                                    GRB.EQUAL, 0), xs, ws)
    # TS constraint
    m.addConstr(TS,GRB.EQUAL,quicksum(ws))
    
    # The TP constraints
    if len(d) != len(tps):
        print "Internal error: d != tps"
        print d
        print tps
        sys.exit(1)
    else:
        map (lambda i,tp,w: m.addConstr(tp, GRB.EQUAL, d[i][1] * TS + d[i][0] * w), 
             d.keys(), tps,ws)
        
    # Finally, the reaction time constraints
    if len(d) != len(ws):
        print "Internal error: d != tps"
        print d
        print ws
        sys.exit(1)
    else:
        map (lambda x,tp,w: m.addConstr(tp <= w * x[2]),d.values(), tps, ws)
        
    # Finally, you can now optimize the model!
    m.optimize()
    
    # Print the results if the solution is optimal
    if m.status == GRB.status.OPTIMAL:
        print '\n'
        for i in xrange(0,len(d)):
            print 'TP'+str(i), ':', tps[i].x/ws[i].x
            print 'x'+str(i), ':', xs[i].x
            print 'CYC'+str(i), ':', ws[i].x
            print '\n'

        # TS
        print 'TS:', TS.x , '\n'


except GurobiError as inst:
    print inst
