# build the database
# make sure you have sqlite3 installed on your machine

sqlite3 games.db "CREATE TABLE pc_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE ps3_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE ps4_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE xbox360_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE xboxOne_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE wii_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE wiiU_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
sqlite3 games.db "CREATE TABLE ids (id INTEGER PRIMARY KEY, appid INTEGER, name TEXT);"


