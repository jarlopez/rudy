#!/usr/bin/env bash
HOSTNAME=localhost
PORT=8080

echo  "===============================[  20 req/sec for 10s ]==============================="
httperf --hog --server $HOSTNAME --uri "/" --num-conn  200 --num-call 1  --timeout 5 --rate  20 --port $PORT
echo  "===============================[  50 req/sec for 10s ]==============================="
httperf --hog --server $HOSTNAME --uri "/" --num-conn  500 --num-call 1  --timeout 5 --rate  50 --port $PORT
echo  "===============================[  100 req/sec for 10s ]==============================="
httperf --hog --server $HOSTNAME --uri "/" --num-conn  1000 --num-call 1  --timeout 5 --rate  100 --port $PORT
echo  "===============================[  250 req/sec for 10s ]==============================="
httperf --hog --server $HOSTNAME --uri "/" --num-conn  2500 --num-call 1  --timeout 5 --rate  250 --port $PORT
echo  "===============================[  500 req/sec for 10s ]==============================="
httperf --hog --server $HOSTNAME --uri "/" --num-conn  5000 --num-call 1  --timeout 5 --rate  500 --port $PORT
# echo  "===============================[ 1000 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 10000 --num-call 1  --timeout 5 --rate 1000 --port $PORT
# echo  "===============================[ 1500 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 15000 --num-call 1  --timeout 5 --rate 1500 --port $PORT
# echo  "===============================[ 1750 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 17500 --num-call 1  --timeout 5 --rate 1750 --port $PORT
# echo  "===============================[ 2000 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 20000 --num-call 1  --timeout 5 --rate 2000 --port $PORT
# echo  "===============================[ 2500 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 25000 --num-call 1  --timeout 5 --rate 2500 --port $PORT
# echo  "===============================[ 3000 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 30000 --num-call 1  --timeout 5 --rate 3000 --port $PORT
# echo  "===============================[ 4000 req/sec for 10s ]==============================="
# httperf --hog --server $HOSTNAME --uri "/" --num-conn 40000 --num-call 1  --timeout 5 --rate 4000 --port $PORT

echo  "===============================[         Done         ]==============================="
