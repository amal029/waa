#!/usr/bin/python


import sys
from gurobipy import *

# try:
usage_message = 'gurobi.sh/gurobi.bat stage1.py/stage2.py <file-name>.txt'



def stage12_optimize(core_num, wcits, wcmts, wcrrs):

    m = Model("stage12")
    weight_ub = 1000.0
    total_rr = sum(wcrrs)
    
    
    core_alloc = {}
    bw = {}
    # Add variables to model
    # Add core allocation binary vars
    for i in range(len(wcmts)):
        # print('for Clock Domain %s: ' % i)
        for j in range(core_num):
            # print('for Clock Domain %s: ' % i)
            core_alloc[i, j] = m.addVar(vtype=GRB.BINARY, name='b_%s_%s' % (i, j))
            for k in range(core_num):
                bw[i,j,k] = m.addVar(lb=0.0, vtype=GRB.INTEGER, name='bw_%s_%s_%s' % (i, j, k))

    # add timing requirement vars
    # prr stands for processor_reaction_time_requirement
    # prr = {}
    valid_core = {}
    weight = {}
    MT_x_TS = {}
    IT_x_weight = {}
    #RR = {}
    RR_x_weight = {}
    for j in range(core_num):
        # prr[j] = m.addVar(vtype=GRB.INTEGER,name='prr_%s' % j)
        valid_core[j] = m.addVar(vtype=GRB.BINARY, obj=1.0, name='valid_core_%s' % j)
        weight[j] = m.addVar(vtype=GRB.INTEGER, name='weight_%s' % j)
        MT_x_TS[j] = m.addVar(vtype=GRB.INTEGER, name='MT_x_TS_%s' % j)
        IT_x_weight[j] = m.addVar(vtype=GRB.INTEGER, name='IT_x_weight_%s' % j)
        #RR[j] = m.addVar(vtype=GRB.INTEGER, name='RR_%s' % j)
        RR_x_weight[j] = m.addVar(vtype=GRB.INTEGER, name='RR_x_weight_%s' % j)

    # Integrate the variables in to the model
    m.update()

    m.write('stage12.lp')
    # set objective

    # add constraint

    
    # c2:
    for i in range(len(wcmts)):
        m.addConstr(quicksum(core_alloc[i, j] for j in range(core_num)), GRB.EQUAL, 1, 'C2_%s' % j)

#     # C3:
#     for j in range(core_num):
#         for i in range(len(wcmts)):
#             # if (core_alloc[i,j] == 1):
#                 # m.addConstr(quicksum(core_alloc[k,j]*wcrts[k] for k in range(len(wcmts))) <= wcrrs[i], 'C3_%s_%s' % (j,i))
#             # m.addConstr(quicksum(core_alloc[k,j]*wcrts[k] for k in range(len(wcmts)))-wcrrs[i] <= quicksum(wcrts[p] for p in range(len(wcmts)))*(1-core_alloc[i,j]), 'C3_%s_%s' % (j,i))
#             m.addConstr(quicksum(core_alloc[k, j] * (wcits[k] + wcmts[k]) for k in range(len(wcmts))) - wcrrs[i] <= quicksum(wcits[p] + wcmts[p] for p in range(len(wcmts))) * (1 - core_alloc[i, j]), 'C3_%s_%s' % (j, i))
#             #m.addConstr(quicksum(core_alloc[k, j] * (wcits[k] + wcmts[k]*(quicksum(valid_core[p] for p in range(core_num)))) for k in range(len(wcmts))) - wcrrs[i] <= quicksum(wcits[p] + wcmts[p] for p in range(len(wcmts))) * (1 - core_alloc[i, j]), 'C3_%s_%s' % (j, i))
    
    
    # C4:
    for j in range(core_num):
        m.addConstr(valid_core[j] <= quicksum(core_alloc[i, j] for i in range(len(wcmts))), 'C4_%s' % j)

    # C5:
    for j in range(core_num):
        for i in range(len(wcmts)):
            m.addConstr(valid_core[j] >= core_alloc[i, j], 'C5_%s_%s' % (i, j))
            
    # C6: C7:
    for j in range(core_num):
        m.addConstr(weight[j] >= valid_core[j], 'C6_%s' % j)
        m.addConstr(weight[j] <= valid_core[j]*weight_ub, 'C7_%s' % j)       
            
    # C8:
    for j in range(core_num):
        for i in range(len(wcmts)):
            for k in range(core_num):
                m.addConstr(bw[i,j,k] <= weight_ub*core_alloc[i,j], 'C8_%s_%s_%s' % (i, j, k))
                m.addConstr(bw[i,j,k] <= weight[k], 'C9_%s_%s_%s' % (i, j, k))
                m.addConstr(bw[i,j,k] >= weight[k] - weight_ub*(1-core_alloc[i,j]), 'C10_%s_%s_%s' % (i, j, k))
    
    # C11:
    for j in range(core_num):
        m.addConstr(MT_x_TS[j]-quicksum(bw[i,j,k]*wcmts[i] for i in range(len(wcmts)) for k in range(core_num)), GRB.EQUAL, 0, 'C11_%s_%s_%s' % (i,j,k))
        m.addConstr(IT_x_weight[j]-quicksum(bw[i,j,j]*wcits[i] for i in range(len(wcmts))), GRB.EQUAL, 0, 'C12_%s_%s_%s' % (i,j,k))
    
    # C13:
    for j in range(core_num):
        for i in range(len(wcmts)):
            m.addConstr(RR_x_weight[j] - wcrrs[i]*weight[j] <= total_rr*weight[j] - total_rr*bw[i,j,j], 'C13_%s_%s_%s' % (i,j,j))
            
            
    # C14:
    for j in range(core_num):
        m.addConstr(MT_x_TS[j] + IT_x_weight[j] <= RR_x_weight[j], 'C14_%s_%s_%s' % (i,j,k))
        
    # C15:
    

    # Finally optimize!!
    m.optimize()
    m.printStats()
    m.printQuality()

    print('\n\n--------------- Printing Optimized Results ---------------')
    print('Objective is: %s ' % m.getObjective().getValue())

    # for v in m.getVars():
    #    print v.varName, v.x

    solution = open('solution_stage12.txt', 'w')
    
    solution.write('Objective is: %s \n' % m.getObjective().getValue())
    for j in range(core_num):
        print('allocated core sign for core %s is %s: ' % (j, valid_core[j]))
        solution.write('allocated core sign for core %s is %s: \n' % (j, valid_core[j]))


    
    stage2_data = {}
    for i in range(len(wcmts)):
        print('for Clock Domain %s: ' % i)
        solution.write('for Clock Domain %s: \n' % i)
        for j in range(core_num):
            print('Processor Allocation for index (%s,%s) is %s: ' % (i, j, core_alloc[i, j]))
            solution.write('Processor Allocation for index (%s,%s) is %s: \n' % (i, j, core_alloc[i, j]))
            if core_alloc[i, j].X == 1:
                if j in stage2_data:
                    stage2_data[j][0] += wcits[i]
                    stage2_data[j][1] += wcmts[i]
                    if wcrrs[i] < stage2_data[j][2]:
                        stage2_data[j][2] = wcrrs[i]
                else: 
                    stage2_data[j] = [wcits[i],wcmts[i],wcrrs[i]]
                print stage2_data
    
    for j in range(core_num):
        print('TDMA slots for core %s is %s: ' % (j, weight[j]))
    
                
    print "stage12_result: "
    solution.write("stage12_result: \n")
    for j in stage2_data.keys():
        print ("core %s: %s; TDMA slot number: %s" % (j,str(stage2_data[j]), weight[j]))
        solution.write("core %s: %s; TDMA slot number: %s\n" % (j,str(stage2_data[j]), weight[j]))

    # write the stage2 file at last 
#     outfile = open('stage2.csv', 'w')
#     for i in stage2_data:
#         outfile.write(','.join(str(x) for x in stage2_data[i])+"\n")
#     
#     outfile.close
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

    f = open('stage1.csv')
    # d = {}
    wcits = []
    wcmts = []
    wcrrs = []
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
            wcrrs.append(lstr[2])
            # print "wcits: "+str(wcits)
            # print "wcmts: "+str(wcmts)
            # print "wcrrs: "+str(wcrrs)
            # d.update({counter : lstr})
            #counter += 1
        else:
            continue
            
    f.close()

    # the file format: wcit, wcmt, wcrrs
    # 10,15,90
    # 25,10,90
    # 20,20,89

    # wcits = [10,25,20]
    # wcmts = [15,10,20]
    # wcrts = [25,35,40]
    # wcrrs = [90,90,89]
    # core_num = 3

    print wcits
    print wcmts
    print wcrrs

    success = stage12_optimize(len(wcits), wcits, wcmts, wcrrs)



except GurobiError:
    print "Error"
