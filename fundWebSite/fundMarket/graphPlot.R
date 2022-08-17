##----------------------------------------------------------------------

# endDate = c()
# lastDate = endDate - months(1)
# 
# dataRange1 = dataRange.function(data,lastDate)
# dataRange2 = dataRange.function(data,endDate)
# 
# betas1 = betaOptim.function(dataRange1)
# betas2 = betaOptim.function(dataRange2)
# betasChanged = betas2 - betas1
# 
# opar <- par(mfrow=c(2,2),oma=c(0,0,2,0),
#             mar=c(2,2,0.5,0.5), 
#             mgp=c(0.5, 0.5, 0), tck=0.005)
# stockHeldNum.function(betas2)
# stockChangedNum.function(betasChanged)
# stockHeldNet.function(betas2,values2)
# stockChangedNum.function(betasChanged,values2)
# mtext(side=3, text='基金持股比例分布', cex=1, outer=T)


# 
# 
# ##------------------------------pie plot--------------------------------
# stockHeldNum.function = function(betas){
#   bars = 1:6
#   bars[1] = sum(betas<0.5)
#   bars[2] = sum(betas>=0.5 & betas<0.6)
#   bars[3] = sum(betas>=0.6 & betas<0.7)
#   bars[4] = sum(betas>=0.7 & betas<0.8)
#   bars[5] = sum(betas>=0.8 & betas<0.9)
#   bars[6] = sum(betas>=0.9)
#   bars = bars/length(betas)
#   
#   pie(bars,
#       main = "基金股票投资比例分布图（按数量）",
#       labels = c("<50%","50%-60%","60%-70%",
#                  "70%-80%","80%-90%",">=90%"),
#       radius = 0.8
#   )
# }
# 
# stockChangedNum.function = function(betasChanged){
#   bars = 1:5
#   bars[1] = sum(betasChanged<-0.026)
#   bars[2] = sum(betasChanged>=-0.026 & betasChanged<-0.006)
#   bars[3] = sum(betasChanged>=-0.006 & betasChanged<0.006)
#   bars[4] = sum(betasChanged>=0.006 & betasChanged<0.026)
#   bars[5] = sum(betasChanged>=0.026)
#   bars = bars/length(betasChanged)
#   
#   pie(bars,
#       main = "基金股票投资比例变动分布图（按数量）",
#       labels = c("减2.6%以上","减0.6%-2.6%","变动0.6%",
#                  "增0.6%-2.6%","增2.6%以上"),
#       radius = 0.8
#   )
#   
# }
# 
# stockHeldNet.function = function(betas,values){
#   bars = 1:6
#   bars[1] = sum(values[betas<0.5])
#   bars[2] = sum(values[betas>=0.5 & betas<0.6])
#   bars[3] = sum(values[betas>=0.6 & betas<0.7])
#   bars[4] = sum(values[betas>=0.7 & betas<0.8])
#   bars[5] = sum(values[betas>=0.8 & betas<0.9])
#   bars[6] = sum(values[betas>=0.9])
#   bars = bars/sum(values)
#   pie(bars,
#       main = "基金股票投资比例分布图（按净资产）",
#       labels = c("<50%","50%-60%","60%-70%",
#                  "70%-80%","80%-90%",">=90%"),
#       radius = 0.8
#   )
# }
# 
# stockChangedNet.function = function(betasChanged,values){
#   bars = 1:5
#   bars[1] = sum(values[betasChanged<-0.026])
#   bars[2] = sum(values[betasChanged>=-0.026 & betasChanged<-0.006])
#   bars[3] = sum(values[betasChanged>=-0.006 & betasChanged<0.006])
#   bars[4] = sum(values[betasChanged>=0.006 & betasChanged<0.026])
#   bars[5] = sum(values[betasChanged>=0.026])
#   bars = bars/sum(values)
#   
#   pie(bars,
#       main = "基金股票投资比例变动分布图（按净资产）",
#       labels = c("减2.6%以上","减0.6%-2.6%","变动0.6%",
#                  "增0.6%-2.6%","增2.6%以上"),
#       radius = 0.8
#   )
# }