-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE DATABASE IF NOT EXISTS pico_outreach_local;
SHOW DATABASES;
GRANT ALL PRIVILEGES ON pico_outreach_local.* TO 'erlend'@'localhost' IDENTIFIED BY 'picoPass2022';