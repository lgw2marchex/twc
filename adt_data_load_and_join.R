WORKING_DIR = "\\\\marchex/home/sea/lwilliams/private/R/adt"

require(sqldf)
setwd(WORKING_DIR)

adt_call_scoring<-read.table("adt_call_scoring.csv",sep=",",header=TRUE)

#sapply(adt_call_scoring,class)
#dim(adt_call_scoring)

adt_call_dna<-read.table("adt_call_dna.csv",sep=",",header=TRUE)

#sapply(adt_call_dna,class)
#dim(adt_call_dna)

adt_calls<-sqldf("select
                 job_name,completion_date,listener_id,listen_date,outcome_name,outcome_name_toplevel,
                 conversation,product_or_service,duration,start_time,is_billable,revenue,distributor,client_name,
                 tracking_phone,dna_classification,assignment_id,prez_level1,prez_level2,category1,category2, converted,
                 a2.* from adt_call_scoring a1
                 inner join adt_call_dna a2
                 on a1.call_id = a2.call_id")

#dim(adt_calls)

adt_fk_call_data<-read.table("adt_fk_call_data.csv",sep=",",header=TRUE)

#sapply(adt_fk_call_data,class)
#dim(adt_fk_call_data)

#sqldf("select id, count(*) from adt_fk_call_data group by id having count(*) > 1")

adt_call_ivr_response<-sqldf("select distinct ID as id, EXTENSION as extension, IVR_DURATION as ivr_duration from adt_fk_call_data")

adt_calls<-sqldf("select a.*, extension, ivr_duration
                 from adt_calls a
                 inner join adt_call_ivr_response r
                 on a.call_id = r.id
                 where ivr_duration is not null")

#dim(adt_calls)

##Checked on dropped records (no r.id) and they're all old (7 or 8, 2013). Safe to use inner join above
#no_ext<-sqldf("select * from adt_calls where extension is null")
#head(no_ext)
#fix(no_ext)

##Adding the 'where ivr_duration is not null' clause causes substantial dropoff...the field appears to have been added to the DB Fall 2014
##Using the filter for now as the more recent data should be better for modeling even though the sample is smaller

##EXTENSION appears to be of no use in modeling. 
##Either there is no billability criteria, or the rules have changed over time or by tracking phone
#sqldf("select extension, count(*), sum(is_billable), sum(is_billable)*1.0/count(*) from adt_calls group by extension order by count(*) desc")

