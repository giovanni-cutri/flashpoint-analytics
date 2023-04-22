import os
import urllib.request
import pandas as pd
import sqlite3

os.mkdir("data")

url = "http://infinity.unstable.life/Flashpoint/Data/flashpoint.sqlite"
filename = "data/flashpoint.sqlite"
urllib.request.urlretrieve(url, filename)

con = sqlite3.connect("data/flashpoint.sqlite")
df = pd.read_sql_query("SELECT * FROM games", con)
con.close()
