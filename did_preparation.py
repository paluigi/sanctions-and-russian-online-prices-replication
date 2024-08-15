# File to pre-process wine and champagne data for DiD
import pandas as pd

include_words = ["Вино", "вино", "Шампанское", "шампанское", "Champagne", "champagne", "Prosecco", "prosecco"]
champagne_words =  ["Шампанское", "шампанское", "Champagne", "champagne"]
prosecco_words = ["Prosecco", "prosecco", "Spumante", "spumante"]

# Read data
data_df = pd.read_csv("data/2-1_alcohol.csv")
# Extract wine and champagne products
wine_df = data_df[data_df["name"].str.contains("|".join(include_words))].reset_index()
# Mark Champagne and Prosecco
wine_df["champagne"] = wine_df["name"].str.contains("|".join(champagne_words))
wine_df["prosecco"] = wine_df["name"].str.contains("|".join(prosecco_words))
# Extract brand
wine_df[["brand", "drop"]] =  wine_df["name"].str.split(" - ", n=1, expand=True)
wine_df.drop(columns=["index", "drop"], inplace=True)# Drop name duplicate and index column
# Mark weeks after Champagne export ban
wine_df["date"] = pd.to_datetime(wine_df["date"])
wine_df["sanction"] = wine_df["date"] > pd.to_datetime("2022-03-15")  # March 15 2022 export ban
# Mark DID variable
wine_df["did"] = wine_df["sanction"] * wine_df["champagne"]
wine_df.to_csv("elaborations/champagne_did_data.csv", index=False)
