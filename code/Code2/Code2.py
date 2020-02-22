import os
import string
v_f1 = open("Ec.fa", 'r')
v_f2 = open("甘肃鼢鼠肝组织liver21_6.csv.csv")
v_m1=  v_f1.readlines()
v_m2 = v_f2.readlines()

test = 2
v_output = open("E:/实验/03.基团筛选/differenceGene/output.txt","w")

for n2 in v_m2:
    n2 = n2.strip()
    for n1 in v_m1:
        n1 = n1.strip()
        if test == 1:
            v_output.write(n1)
            v_output.write('\n')
            test = 2
        elif test != 1:
            if n2 in n1:
                test = 1
                v_output.write(">" + n2)
                v_output.write('\n')
            else:
                test = 2
v_output.close()