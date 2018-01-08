FROM postgres:9.6.5

RUN apt-get update && \
  apt-get install -y wget unzip && \
  wget https://ftp.postgresql.org/pub/projects/pgFoundry/dbsamples/pagila/pagila/pagila-0.10.1.zip -O /tmp/pagila-0.10.1.zip && \
  unzip /tmp/pagila-0.10.1.zip -d /tmp && \
  cat /tmp/pagila-0.10.1/pagila-schema.sql | \
  # ERROR:  language "plpgsql" already exists
  grep -v "CREATE PROCEDURAL LANGUAGE plpgsql" > /docker-entrypoint-initdb.d/pagila_init.sql && \
  cat /tmp/pagila-0.10.1/pagila-data.sql >> /docker-entrypoint-initdb.d/pagila_init.sql

ENTRYPOINT ["docker-entrypoint.sh"]

EXPOSE 5432
CMD ["postgres"]
