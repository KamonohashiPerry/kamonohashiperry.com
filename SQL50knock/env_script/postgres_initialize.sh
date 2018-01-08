#!/bin/bash

set -eu

# checks required commands
command -v docker > /dev/null || { echo "dockerをインストールしてください"; exit 1; }

# kills & removes a postgres_sd container
docker rm -f postgres_sd psql_sd || true

# running postgres on docker
docker network create postgre_default || true

docker build -t postgres_sd:9.6.5 .
docker run --net postgre_default --net-alias postgres_sd --name postgres_sd -d postgres_sd:9.6.5

# psql container
docker pull postgres:9.6.5
sleep 10

cat <<EOS

#================
# login postgres
#================
EOS

while !(docker run -it --net postgre_default --net-alias psql_sd --name psql_sd postgres:9.6.5 psql -U postgres -h postgres_sd -p 5432)
do
  echo "wait PostgreSQL server..."
  docker rm -f psql_sd || true
  sleep 3
done
