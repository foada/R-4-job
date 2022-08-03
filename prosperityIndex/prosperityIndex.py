from datetime import date
import pandas as pd
import os


def readData(file):
    table = pd.read_excel(file)
    dateProsper = table.loc[5:1715,['指标名称', 'Datayes景气度(发布日期)']]
    dateProsper.index = range(len(dateProsper))
    return dateProsper

# def isMonthEnd(ymdTime):
#     monthEnds = ["0131","0331","0430","0531","0630","0731",
#                     "0831","0930","1031","1130","1231"]
#     ymd = ymdTime.split()[0].split("-")
#     md = ymd[1]+ymd[2]
#     if (ymd[0]%4 == 0) & (md in monthEnds+["0229"]):
#         return True
#     elif (ymd[0]%4!=0) & (md in monthEnds+["0228"]):
#         return True
#     return False
        
# def extraProsper(dateProsper):
#     singleProsper = pd.DataFrame(columns=["mdate","prosperity"])
#     for i in range(len(dateProsper)):
#         mdate = dateProsper.iloc[i,0]
#         if isMonthEnd(str(mdate)):
#             singleProsper.loc[len(singleProsper)] = [mdate,
#                     dateProsper.at[i,"Datayes景气度(发布日期)"]]
#     return singleProsper

def prosperIndex():
    fileFolder = r"E:/MyFiles/Programming/R/ProsperityIndex"
    totalProsper = pd.DataFrame(columns=["mdate"])
    for file in os.listdir(fileFolder):
        dateProsper = readData(os.path.join(fileFolder,file))
        singleProsper = dateProsper[pd.notna(dateProsper.iloc[:,1])]
        totalProsper[file.split(".")[0]] = singleProsper.iloc[:,1]
    totalProsper["mdate"] = singleProsper.iloc[:,0]
    totalProsper.index = range(len(totalProsper))
    return totalProsper