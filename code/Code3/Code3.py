import re
with open("diffOf21and6.txt", "r", encoding = "utf-8") as f:
    with open("diffOf21and6.csv", "a+", encoding = "utf-8") as m:
        m.write("Code,Name,Num\n")
    for i in f:
        i = i.replace(",", "，")
        vCode = re.search("\d{5}", i, re.S).group(0)
        vName = re.search("[a-zA-Z][\d\D]*[a-zA-Z]", i, re.S).group(0)
        vNum = re.search("\(([\d]*)\)", i, re.S).group(1)
        with open("diffOf21and6.csv", "a+", encoding = "utf-8") as m:
            m.write(vCode)
            m.write(",")
            m.write(vName)
            m.write(",")
            m.write(vNum)
            m.write("\n")