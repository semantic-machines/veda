#!/bin/bash

QUEUE_PATH=data/queue
QUEUE_NAME=individuals-flow
SFX_CHUNK=_current_chunk
SFX_INFO=_info_push_0
SFX_QUEUE=_queue_0

LOCK_FILE="$QUEUE_PATH/$QUEUE_NAME"

CONSUMER_NAME[1]="CCUS"
CONSUMER_NAME[2]="fanout_email0"
CONSUMER_NAME[3]="fanout_sql_lp0"
CONSUMER_NAME[4]="fanout_sql_np0"
CONSUMER_NAME[5]="fulltext_indexer0"
CONSUMER_NAME[6]="fulltext_indexer1"
CONSUMER_NAME[7]="ltr_scripts0"
CONSUMER_NAME[8]="scripts_lp0"
CONSUMER_NAME[9]="scripts_main0"
CONSUMER_NAME[10]="user_modules_tool0"

for i in "${CONSUMER_NAME[@]}"; do

    CUR_COUNT=$(./vqueuectl consumer_unread_counter individuals-flow data/queue $i)
    echo $i $CUR_COUNT

done
