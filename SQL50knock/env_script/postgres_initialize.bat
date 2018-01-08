@echo off

:check docker
where docker
if not %errorlevel% == 0 ( echo "Docker がインストールされていないようです" & exit /b 1 )

:check docker daemon
docker info
if not %errorlevel% == 0 ( echo "Docker daemonが起動していないようです" & exit /b 1 )

:kills & removes a postgres_sd container
docker rm -f postgres_sd

:running postgres on docker
docker network create postgre_default

docker build -t postgres_sd:9.6.5 .
docker run --net postgre_default --net-alias postgres_sd --name postgres_sd -d postgres_sd:9.6.5

:sql container
docker pull postgres:9.6.5
timeout 7 /NOBREAK > NUL

echo #================
echo # login postgres
echo #================

:loop
timeout 3 /NOBREAK > NUL
docker rm -f  psql_sd
docker run -i -t --net postgre_default --net-alias psql_sd --name psql_sd postgres:9.6.5 psql -U postgres -h postgres_sd -p 5432
if not %errorlevel% == 0 ( echo wait PostgreSQL server... & goto loop )