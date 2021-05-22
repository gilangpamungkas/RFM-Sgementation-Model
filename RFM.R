library("rfm")
RFM_data_orders <- read.csv(file = "RFMLogistikNew.csv",header=TRUE)
head(RFM_data_orders)
RFM_data_orders
nrow(RFM_data_orders)
library(dplyr)
RFM_data_orders1 <- tbl_df(RFM_data_orders)
nrow(RFM_data_orders1)
RFM_data_orders1$IDMasking <- as.character(RFM_data_orders1$IDMasking)
RFM_data_orders1$trxamount <- as.numeric(RFM_data_orders1$trxamount)
RFM_data_orders1$paid_at <- as.Date(RFM_data_orders1$paid_at, format="%m/%d/%Y" , origin="1900-01-01")
head(RFM_data_orders1)
analysis_date <- lubridate::as_date("2021-02-17")
rfm_result <- rfm_table_order(RFM_data_orders1, IDMasking, paid_at, trxamount, analysis_date,4,4,4)
rfm_result
nrow(rfm_result)
#Customer Segmentation

champions<- c(444)
loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
potential_loyalist <-c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
recent_customers <- c(411)
promising <- c(311, 312, 313, 331)
needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
about_to_sleep <- c(211)
at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
cant_lose <- c(134,143,144,234,242,243,244)
hibernating <- c(141)
lost <- c(111)

rfm_scores<-as.vector(rfm_result$rfm$rfm_score)
rfm_scores[which(rfm_result$rfm$rfm_score %in% champions)]="Champions"
rfm_scores[which(rfm_result$rfm$rfm_score %in% potential_loyalist)] = "Potential Loyalist"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% loyal_customers)] = "Loyal Customers"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% recent_customers)] = "Recent Customers"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% promising)] = "Promising"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% needing_attention)] = "Customer Needing Attention"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% about_to_sleep)] = "About to Sleep"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% at_risk)] = "At Risk"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% cant_lose)] = "Can't Lose Them"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% hibernating)] = "Hibernating"
rfm_scores[which(rfm_result$rfm$rfm_score  %in% lost)] = "Lost"

rfm_result
#converting to csv
write.table(rfm_result$rfm,file="rfm_results.csv",sep=",",append=FALSE,row.names = FALSE)
RFM_data_reuse <- read.csv(file = "rfm_results.csv",header=TRUE)
RFM_data_reuse


customer_sement<-data.frame(cus_seg=rfm_scores)
customer_sement%>%count(cus_seg)%>%arrange(desc(n))%>%rename(cus_seg = cus_seg, Count = n)


rfm_blended <- cbind(RFM_data_reuse, customer_sement)
write.table(rfm_blended,file="rfm_blended.csv",sep=",",append=FALSE,row.names = FALSE)
rfm_blended
rfm_heatmap(rfm_result)
rfm_bar_chart(rfm_result)
rfm_histograms(rfm_result)
rfm_order_dist(rfm_result)
rfm_order_dist(rfm_result)
rfm_rm_plot(rfm_result)
rfm_fm_plot(rfm_result)
rfm_rf_plot(rfm_result)
customer_sement


library(ggplot2)
ggplot(data = customer_sement) + aes(x = cus_seg, fill = cus_seg)+ geom_bar() + labs(title = "Customer Segmentation", x = "Segment", y = "Total Customer") + coord_flip()+ theme_minimal()
