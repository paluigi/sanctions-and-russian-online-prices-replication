import calendar
import sqlite3
import os
from datetime import datetime
import pandas as pd


db_dir = "elaborations"

cpi_db = "cpi.db"


official_data_dir = "official_data"
official_data_file = "russia_cpi_v2.ods"

# Instantiate DB
con_cpi = sqlite3.connect(os.path.join(db_dir, cpi_db))
cur_cpi = con_cpi.cursor()

# Read and elaborate official CPI file
official_df = pd.read_excel(
    os.path.join(official_data_dir, official_data_file),
    sheet_name="CPI_Level",
    header=0,
    skiprows=range(1, 2),
    engine="odf",
    na_values="#VALUE!",
)
# Process Rosstat dates to align with last sunday
official_df["Date"] = pd.to_datetime(official_df["Date"])
official_df["Year"] = official_df.apply(lambda row: row["Date"].year, axis=1)
official_df["Month"] = official_df.apply(lambda row: row["Date"].month, axis=1)
official_df["Last_Sun"] = official_df.apply(
    lambda row: max(
        week[-1] for week in calendar.monthcalendar(row["Year"], row["Month"])
    ),
    axis=1,
)

official_df["Date"] = official_df.apply(
    lambda row: f'{row["Year"]}-{row["Month"]:02}-{row["Last_Sun"]}', axis=1
)
official_df["Date"] = pd.to_datetime(official_df["Date"])

official_df.drop(
    columns=[
        "Year",
        "Month",
        "Last_Sun",
    ],
    inplace=True,
)


# Get list of tables from DB
cur_cpi.execute("SELECT name FROM sqlite_master WHERE type='table';")
cpi_table_list = [t[0] for t in cur_cpi.fetchall()]

# Merge CPI tables with official data
for c in official_df.columns:
    if c in cpi_table_list:
        temp_df = official_df[["Date", c]]
        temp_df.columns = ["date", "official"]
        data_df = pd.read_sql_query(f"SELECT * FROM '{c}'", con_cpi)
        data_df["date"] = pd.to_datetime(data_df["date"])
        # For reworking data, drop existing official columns
        if "official" in data_df.columns:
            data_df = data_df.drop(columns=["official"])
        data_df = pd.merge_ordered(data_df, temp_df, on="date", how="outer")
        data_df["date"] = data_df["date"].dt.strftime("%Y-%m-%d")
        data_df.to_sql(name=c, con=con_cpi, if_exists="replace", index=False)

