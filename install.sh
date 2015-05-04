# build the database
# make sure you have sqlite3 installed on your machine

sqlite3 games.db "CREATE TABLE games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT);"
sqlite3 games.db "CREATE TABLE ids (id INTEGER PRIMARY KEY, appid INTEGER, name TEXT);"
sqlite3 games.db "CREATE TABLE valid_steam (id INTEGER PRIMARY KEY, name TEXT, price REAL);"



