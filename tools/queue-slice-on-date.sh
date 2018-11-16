#!/bin/bash

TIMESTAMP=`date +%Y-%m-%d_%H_%M`

mkdir data-log

SLICE_PATH=data-log
QUEUE_PATH=data/queue
QUEUE_NAME=individuals-flow
SFX_CHUNK=_current_chunk
SFX_INFO=_info_push_0
SFX_QUEUE=_queue_0

LOCK_FILE="$QUEUE_PATH/$QUEUE_NAME"
LOCK_FILE+="_queue.lock"

if [ -e $LOCK_FILE ] 
then
    echo "queue busy"
    exit -1
fi

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

TOTAL_COUNT=0

for i in "${CONSUMER_NAME[@]}"; do

    CUR_COUNT=$(./vqueuectl consumer_unread_counter individuals-flow data/queue $i)
    echo $i $CUR_COUNT
    TOTAL_COUNT=$((TOTAL_COUNT + $CUR_COUNT))

done

if ((TOTAL_COUNT == 0)); then

    echo 'WARN! all subscribers have processed all messages, this queue can be deleted.'
    mkdir $SLICE_PATH/$TIMESTAMP

    CHK_FILE="$QUEUE_PATH/$QUEUE_NAME$SFX_CHUNK"
    echo move $CHK_FILE to $SLICE_PATH//$TIMESTAMP/$QUEUE_NAME$SFX_CHUNK
    mv  $CHK_FILE $SLICE_PATH//$TIMESTAMP/$QUEUE_NAME$SFX_CHUNK

    INFO_FILE="$QUEUE_PATH/$QUEUE_NAME$SFX_INFO"
    echo move $INFO_FILE to $SLICE_PATH//$TIMESTAMP/$QUEUE_NAME$SFX_INFO
    mv  $INFO_FILE $SLICE_PATH//$TIMESTAMP/$QUEUE_NAME$SFX_INFO
    
    QUEUE_FILE="$QUEUE_PATH/$QUEUE_NAME$SFX_QUEUE"
    echo move $QUEUE_FILE to $SLICE_PATH//$TIMESTAMP/$QUEUE_NAME$SFX_QUEUE
    mv  $QUEUE_FILE $SLICE_PATH//$TIMESTAMP/$QUEUE_NAME$SFX_QUEUE

    rm $QUEUE_PATH/*pop*
fi

