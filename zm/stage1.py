#!/usr/bin/python


import sys
from gurobipy import *

# try:
usage_message = 'gurobi.sh/gurobi.bat stage1.py/stage2.py <file-name>.txt'



def stage1_optimize(core_num, wcits, wcmts, rrs):

    m = Model("stage1")

    core_alloc = {}
    # Add variables to model
    # Add core allocation binary vars
    for i in range(len(wcmts)):
        # print('for Clock Domain %s: ' % i)
        for j in range(core_num):
            # print('for Clock Domain %s: ' % i)
            core_alloc[i, j] = m.addVar(vtype=GRB.BINARY, name='b_%s_%s' % (i, j))

    # add timing requirement vars
    # prr stands for processor_reaction_time_requirement
    # prr = {}
    valid_core = {}
    for j in range(core_num):
        # prr[j] = m.addVar(vtype=GRB.INTEGER,name='prr_%s' % j)
        valid_core[j] = m.addVar(vtype=GRB.BINARY, obj=1.0, name='valid_core_%s' % j)

    

    # Integrate the variables in to the model
    m.update()

    m.write('stage1.lp')
    # set objective

    # add constraint

    # c2:
    for i in range(len(wcmts)):
        m.addConstr(quicksum(core_alloc[i, j] for j in range(core_num)), GRB.EQUAL, 1, 'C2_%s' % j)

    # C3:
    for j in range(core_num):
        for i in range(len(wcmts)):
            # if (core_alloc[i,j] == 1):
                # m.addConstr(quicksum(core_alloc[k,j]*wcrts[k] for k in range(len(wcmts))) <= rrs[i], 'C3_%s_%s' % (j,i))
            # m.addConstr(quicksum(core_alloc[k,j]*wcrts[k] for k in range(len(wcmts)))-rrs[i] <= quicksum(wcrts[p] for p in range(len(wcmts)))*(1-core_alloc[i,j]), 'C3_%s_%s' % (j,i))
            m.addConstr(quicksum(core_alloc[k, j] * (wcits[k] + wcmts[k]) for k in range(len(wcmts))) - rrs[i] <= quicksum(wcits[p] + wcmts[p] for p in range(len(wcmts))) * (1 - core_alloc[i, j]), 'C3_%s_%s' % (j, i))
#m.addConstr(quicksum(core_alloc[k, j] * (wcits[k] + wcmts[k]*(quicksum(valid_core[p] for p in range(core_num)))) for k in range(len(wcmts))) - rrs[i] <= quicksum(wcits[p] + wcmts[p] for p in range(len(wcmts))) * (1 - core_alloc[i, j]), 'C3_%s_%s' % (j, i))


    # C4:
    for j in range(core_num):
        m.addConstr(valid_core[j] <= quicksum(core_alloc[i, j] for i in range(len(wcmts))), 'C4_%s' % j)

    # C5:
    for j in range(core_num):
        for i in range(len(wcmts)):
            m.addConstr(valid_core[j] >= core_alloc[i, j], 'C5_%s_%s' % (j, i))

    # Finally optimize!!
    m.optimize()
    m.printStats()
    m.printQuality()

    print('\n\n--------------- Printing Optimized Results ---------------')
    print('Objective is: %s ' % m.getObjective().getValue())

    # for v in m.getVars():
    #    print v.varName, v.x

    solution = open('solution.txt', 'w')
    for j in range(core_num):
        print('allocated core sign for core %s is %s: ' % (j, valid_core[j]))
        solution.write('allocated core sign for core %s is %s: ' % (j, valid_core[j]))
        solution.write("\n")

    
    stage2_data = {}
    for i in range(len(wcmts)):
        print('for Clock Domain %s: ' % i)
        solution.write('for Clock Domain %s: ' % i)
        solution.write("\n")
        for j in range(core_num):
            print('Processor Allocation for index (%s,%s) is %s: ' % (i, j, core_alloc[i, j]))
            solution.write('Processor Allocation for index (%s,%s) is %s: ' % (i, j, core_alloc[i, j]))
            solution.write("\n")
            if core_alloc[i, j].X == 1:
                if j in stage2_data:
                    stage2_data[j][0] += wcits[i]
                    stage2_data[j][1] += wcmts[i]
                    if rrs[i] < stage2_data[j][2]:
                        stage2_data[j][2] = rrs[i]
                else: 
                    stage2_data[j] = [wcits[i],wcmts[i],rrs[i]]
                print stage2_data

    print stage2_data

    # write the stage2 file at last 
    outfile = open('stage2.txt', 'w')
    for i in stage2_data:
        outfile.write(','.join(str(x) for x in stage2_data[i])+"\n")
    
    outfile.close
    solution.close

    if m.status == GRB.status.OPTIMAL:
        return True
    else:
        return False


try:

    # if len(sys.argv) != 2:
    #    print usage_message
    #    sys.exit(1)

    # f = open(str(sys.argv[1]))

    f = open('stage1.txt')
    # d = {}
    wcits = []
    wcmts = []
    rrs = []
    #counter = 0
    for line in f:
        if line.strip():
            lstr = line.split(',')
            lstr = map (lambda x: int(x.rstrip('\n\r')), lstr)
    
            if len(lstr) != 3:
                print "there should be three numbers per line: "
                print lstr
                sys.exit(1)
            wcits.append(lstr[0])
            wcmts.append(lstr[1])
            rrs.append(lstr[2])
            # print "wcits: "+str(wcits)
            # print "wcmts: "+str(wcmts)
            # print "rrs: "+str(rrs)
            # d.update({counter : lstr})
            #counter += 1
        else:
            continue
            
    f.close()

    # the file format
    # 10,15,90
    # 25,10,90
    # 20,20,89

    # wcits = [10,25,20]
    # wcmts = [15,10,20]
    # wcrts = [25,35,40]
    # rrs = [90,90,89]
    # core_num = 3

    print wcits
    print wcmts
    print rrs

    success = stage1_optimize(len(wcits), wcits, wcmts, rrs)



except GurobiError:
    print "Error"
