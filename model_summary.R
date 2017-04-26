## Model summary function

model_summary<-function(model,geography,period,experiment){
    model_stats<-data.frame(
        Experiment = rep(experiment,summary(model)$df[1]),
        Attribute  = rownames(summary(model)$coefficients),
        Beta.Coeff = summary(model)$coefficients[,1],
        P.Value    = summary(model)$coefficients[,4],
        Adj.R2     = rep(summary(model)$adj.r.squared,summary(model)$df[1]),
        DF         = rep(summary(model)$df[2],summary(model)$df[1]),
        Geography  = rep(geography,summary(model)$df[1]),
        Data.range = rep(period,summary(model)$df[1])
        )
    rownames(model_stats)<-NULL
    return(model_stats)
}

all_model_stats[all_model_stats$Attribute %in% c("int_rate","UNEMP.RT.BEGIN_DT","UNEMP.RT_ISS","fico_range_low"),]