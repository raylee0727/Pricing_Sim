import os
 
dataPath = r'C:\Users\Andy\Desktop\洪災\中正'
 
print(dataPath)
allName = os.listdir(dataPath)
 
allTestDataName = []
for filename in os.listdir(dataPath):
    if filename.endswith('.png') and not('pre' in filename):#文件名中不包含'pre'字符串
        #print(filename)
        allTestDataName.append(filename)
 
 
allTestDataName.sort(key= lambda x:int(x[:-4]))
print(allTestDataName)