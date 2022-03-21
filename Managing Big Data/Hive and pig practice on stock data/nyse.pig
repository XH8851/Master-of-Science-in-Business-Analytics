dividends = LOAD 's3://hm3xiao/work1/NYSE_dividends' as (org:chararray, company:chararray, dmy:chararray, dividend:float);
daily = LOAD 's3://hm3xiao/work1/NYSE_daily' as (org:chararray, company:chararray, dmy:chararray, open:float, high:float, low:float, close:float, code:int, adjClose:float);

/* 2.2.2. Join daily and dividends on stock and date */

df222 = JOIN dividends BY (company, dmy), daily BY (company, dmy);



/* 2.2.3. Calculate dividend/close_price */

df223 = foreach df222 generate *, dividend/close as divOverClose;



/* 2.2.4. Find stock ticker and date for minimum and maximum value of dividend/close_price */

df224Min = order df223 by divOverClose asc;
df224Min = limit df224Min 1;
df224Min = FOREACH df224Min GENERATE dividends::company, dividends::dmy;
dump df224Min;

df224Max = order df223 by divOverClose desc;
df224Max = limit df224Max 1;
df224Max = FOREACH df224Max GENERATE dividends::company, dividends::dmy;
dump df224Max;