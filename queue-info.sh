#!/bin/bash

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_CCUS`
COUNT_CCUS=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_fanout_email`
COUNT_FANOUT_EMAIL=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_fanout_sql_lp`
COUNT_FANOUT_SQL_LP=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_fanout_sql_np`
COUNT_FANOUT_SQL_NP=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_fulltext_indexer`
COUNT_FTI=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_ltr_scripts`
COUNT_LTR_SCRIPTS=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_scripts_lp`
COUNT_SCRIPTS_LP=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_pop_scripts_main`
COUNT_SCRIPTS_MAIN=${array[4]}

IFS=';' read -r -a array <<< `cat data/queue/individuals-flow_info_push`
COUNT_PUT=${array[3]}

echo count put: $COUNT_PUT
echo ccus: $(($COUNT_PUT - $COUNT_CCUS))
echo fanout-email: $(($COUNT_PUT - $COUNT_FANOUT_EMAIL))
echo fanout-sql-lp: $(($COUNT_PUT - $COUNT_FANOUT_SQL_LP))
echo fanout sql-lp: $(($COUNT_PUT - $COUNT_FANOUT_SQL_NP))
echo fulltext-indexer: $(($COUNT_PUT - $COUNT_FTI))
echo ltr-scripts: $(($COUNT_PUT - $COUNT_LTR_SCRIPTS))
echo scripts-lp: $(($COUNT_PUT - $COUNT_SCRIPTS_LP))
echo scripts-main: $(($COUNT_PUT - $COUNT_SCRIPTS_MAIN))
