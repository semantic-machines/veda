#!/bin/bash

#host=localhost
#port=8080

host=172.17.35.149
port=8080

#host=172.17.1.169
#port=8087

#query=/get_individual?uri=td%3ARomanKarpov
#query="/get_individual?uri=2904ec59-c56e-4652-a124-0790c01c60b9"

#Сложный типизированный запрос (результатов - 3)
#'v-s:hasAppointmentTo' == 'ap:bd15f297-f1a0-4c1a-ae42-d0c06b22d8be' && 'mnd-s:content'=='бычин*' && 'rdf:type'=='mnd-s-cmn:Note' && 'v-s:author.rdfs:label'=='короб*'
query=/query?query=%27v-s%3AhasAppointmentTo%27%3D%3D%27ap%3Abd15f297-f1a0-4c1a-ae42-d0c06b22d8be%27+%26%26+%27mnd-s%3Acontent%27%3D%3D%27%D0%B1%D1%8B%D1%87%D0%B8%D0%BD*%27+%26%26+%27rdf%3Atype%27%3D%3D%27mnd-s-cmn%3ANote%27+%26%26+%27v-s%3Aauthor.rdfs%3Alabel%27%3D%3D%27%D0%BA%D0%BE%D1%80%D0%BE%D0%B1*%27

#коробейников николай (результатов - 393)
#query=/query?query=%27*%27%3D%3D%27%D0%BA%D0%BE%D1%80%D0%BE%D0%B1%D0%B5%D0%B9%D0%BD%D0%B8%D0%BA%D0%BE%D0%B2*%27%26%26%27*%27%3D%3D%27%D0%BD%D0%B8%D0%BA%D0%BE%D0%BB%D0%B0%D0%B9*%27

#хвостикова (результатов - 30)
#query=/query?query=%27*%27%3D%3D%27%D1%85%D0%B2%D0%BE%D1%81%D1%82%D0%B8%D0%BA%D0%BE%D0%B2%D0%B0*%27

#karpov (результатов - 30)
#query=/query?query=%27*%27%3D%3D%27%karpov*%27

auth=`curl -s "$host:$port/authenticate?login=karpovr&password=a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"`
ticket=`echo $auth | cut -d'"' -f 4`
query="$query&ticket=$ticket"

low_rate=20
high_rate=200
rate_step=20
num_conn=500
num_call=100
timeout=5

autobench --single_host --host1 $host --port1 $port --uri1 $query --low_rate $low_rate --high_rate $high_rate --rate_step $rate_step --num_conn $num_conn --num_call $num_call --timeout $timeout
