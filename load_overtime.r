library(stats)

#prefix = "wecc240"
#csv_file = "data/wecc240_load_profiles.csv"

prefix = "rts96"
csv_file = "data/pglib_opf_case73_ieee_rts_load_profiles.csv"

args = commandArgs(TRUE)
if(length(args) == 5){
    prefix = args[[1]]
    csv_file = args[[2]]
}
print(prefix)
print(csv_file)

st_data = read.csv(csv_file, header=TRUE, stringsAsFactors=FALSE)
print(nrow(st_data))

# explicit tz needed to not drop hours
st_data$time = as.POSIXct(st_data$time, tz="UTC")

load_cols = 2:ncol(st_data)
load_avg = rowMeans(st_data[,load_cols])

#print(load_avg)

#field_col = "darkgoldenrod2"
#coupler_col = "cornflowerblue" #rgb(180,20,50)


file_name = paste(prefix,"_spectrum_all.pdf",sep="")
pdf(file_name, pointsize=14, width=14, height=7)

    load_avg_ts = as.ts(load_avg, start=1, end=length(load_avg), frequency=1)
    spectrum(load_avg_ts, log="no")

    spectrum(load_avg_ts, xlim=c(0.0, 0.1), log="no")

    spectrum(load_avg_ts, xlim=c(0.0, 0.1), log="no")
    #abline(v=seq(from=0.0, to=0.5, by=0.01), lw=1, col=rgb(0.7,0.7,0.7,0.7))

    #abline(v=0.00023, lw=2, col="cornflowerblue") # 2 hour (big peak)
    #abline(v=0.00046, lw=2, col="cornflowerblue") #  4 hour
    #abline(v=0.00068, lw=2, col="cornflowerblue") #  0.25 day
    #abline(v=0.00137, lw=2, col="cornflowerblue") #  0.5 day
    #abline(v=0.00273, lw=2, col="cornflowerblue") #  1 day
    abline(v=0.00546, lw=2, col="cornflowerblue") #   2 day
    abline(v=0.04098, lw=2, col="cornflowerblue") #  15 days
    abline(v=0.08196, lw=2, col="cornflowerblue") #  30 days
    #abline(v=0.16393, lw=2, col="cornflowerblue") #  60 days
    #abline(v=0.16393, lw=2, col="cornflowerblue")
dev.off()

file_name = paste(prefix,"_timeseries_all.pdf",sep="")
pdf(file_name, pointsize=14, width=14, height=7)

    xlim = c(min(st_data$time), max(st_data$time))
    ylim = c(0, max(st_data[,load_cols]))

    plot(xlim, ylim, xlab="Time", ylab="Load (MW)", main=paste("Load Time Series Overtime (n=",length(load_cols),")", sep=""), typ="n")
    #abline(v=time_index, col=rgb(0.7,0.7,0.7,0.7))

    for(col in load_cols){
        points(st_data$time, st_data[,col], typ="l")
    }
    #points(afmf_data$time, afmf_data$value, typ="l", lw=3, col=field_col)
    #points(fmc_data$time, fmc_data$value, typ="l", lw=3, col=coupler_col)
    #points(afmc_data$time, afmc_data$value, typ="l", lw=3, col=coupler_col)

    points(st_data$time, load_avg, typ="l", lw=3, col="darkgoldenrod2")

    legend("topright", c("average"), col=c("darkgoldenrod2"), lw=c(3), bg="white")

dev.off()


time_index = seq(from=round(min(st_data$time),"days"), to=max(st_data$time), by="days")
#print(time_index)
start_time = time_index[1]
end_time = time_index[15]
day_breaks = time_index[1:15]
#print(day_breaks)

st_data_02w = subset(st_data, time >= start_time & time <= end_time)
#print(st_data_02w[1:2,])
#print(st_data_02w[,load_cols])
load_avg_02w = rowMeans(st_data_02w[,load_cols])

file_name = paste(prefix,"_timeseries_02w.pdf",sep="")
pdf(file_name, pointsize=14, width=14, height=7)

    xlim = c(min(st_data_02w$time), max(st_data_02w$time))
    ylim = c(0, max(st_data_02w[,load_cols]))

    plot(xlim, ylim, xlab="Time", ylab="Load (MW)", main=paste("Load Time Series Overtime (n=",length(load_cols),")", sep=""), typ="n")
    abline(v=day_breaks, lw=2, col=rgb(0.7,0.7,0.7,0.7))

    for(col in load_cols){
        points(st_data_02w$time, st_data_02w[,col], typ="l")
    }
    points(st_data_02w$time, load_avg_02w, typ="l", lw=3, col="darkgoldenrod2")

    legend("topright", c("average"), col=c("darkgoldenrod2"), lw=c(3), bg="white")


    plot(xlim, ylim, xlab="Time", ylab="Load (MW)", main=paste("Load Time Series Overtime", sep=""), typ="n")
    abline(v=day_breaks, lw=2, col=rgb(0.7,0.7,0.7,0.7))

    points(st_data_02w$time, load_avg_02w, typ="l", lw=3, col="darkgoldenrod2")

dev.off()


file_name = paste(prefix,"_load_hist.pdf",sep="")
pdf(file_name, pointsize=14, width=7, height=7)

    x = as.numeric(st_data_02w[1,load_cols])
    hist(x, breaks=20, xlab="Load (MW)", ylab="Frequency", main="Load Distribution")

dev.off()


