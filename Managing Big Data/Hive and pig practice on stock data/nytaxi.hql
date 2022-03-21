



-- 3 Submit the above hive queries in text file (e.g. nytaxi.hql) file

-- 3.1. Upload tripdata.csv from canvas to a folder (hive) in your S3 bucket

-- 3.2. Create external table nyTaxi with

hive

create table nyTaxi(

	VendorID int,
	lpep_pickup_datetime string,
	lpep_dropoff_datetime string,
	store_and_fwd_flag string,
	RatecodeID int,
	PULocationID int,
	DOLocationID int,
	passenger_count int,
	trip_distance double,
	fare_amount double,
	extra double,
	mta_tax double,	
	tip_amount double, 
	tolls_amount double,
	ehail_fee double,
	improvement_surcharge double,
	total_amount double,
	payment_type int,
	trip_type int

)
row format delimited FIELDS TERMINATED BY ','
lines terminated by '\n'
LOCATION 's3://hm3xiao/hive/'
tblproperties("skip.header.line.count"="1");

LOAD DATA INPATH 's3://hm3xiao/tripdata/tripdata.csv' overwrite INTO TABLE nyTaxi; 


select * from nyTaxi;



-- 3.3. Get distinct rate_code_id from the table 

select distinct RatecodeID from nyTaxi;



-- 3.4. Show all rows/columns where rate_code_id = 1 

select * from nyTaxi where RatecodeID = 1;



