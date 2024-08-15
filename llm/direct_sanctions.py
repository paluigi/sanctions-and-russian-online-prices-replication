import os
import sqlite3
import pandas as pd
from qdrant_client import models, QdrantClient
from sentence_transformers import SentenceTransformer


# Setup
db_dir = "llm"

db_name = "product_classification.db"
# Instantiate DB
con_cpi = sqlite3.connect(os.path.join(db_dir, db_name))
cur_cpi = con_cpi.cursor()

# Load sanctions list by HS6 code and write to DB
sanctions_df = pd.read_csv("llm/sanctions_hs6_desc.csv", dtype=str) # This file derives from an original list which belongs to the authors
                                                                    # of the paper: The Eurasian Roundabout: Trade
                                                                    # Flows Into Russia Through the Caucasus and Central Asia.
                                                                    # Maxim Chupilkin, Beata Javorcik, and Alexander Plekhanov
                                                                    # and it is not included in the replication package
sanctions_df["EU_sanction"] = sanctions_df["EU_sanction"].fillna(value=0)
sanctions_df = sanctions_df.rename(columns={"EU_sanction": "sanctions", "Code": "code", "Date": "date"})
sanctions_df.to_sql(name="sanctions", con=con_cpi, if_exists="replace", index=False)
# Load sanctions data into Qdrant
sanctions_dict = (
    sanctions_df[["code", "sanctions", "description"]]
    .to_dict(orient="records")
    )
    
qdrant = QdrantClient(path="llm/sanctions.qd")
encoder = SentenceTransformer("paraphrase-multilingual-mpnet-base-v2")
qdrant.recreate_collection(
    collection_name="sanctions",
    vectors_config=models.VectorParams(
        size=encoder.get_sentence_embedding_dimension(),  # Vector size is defined by used model
        distance=models.Distance.COSINE,
    ),
)

qdrant.upload_points(
    collection_name="sanctions",
    points=[
        models.Record(
            id=idx, vector=encoder.encode(sanct["description"]).tolist(), payload=sanct
        )
        for idx, sanct in enumerate(sanctions_dict)
    ],
)
# Cleanup
del(sanctions_dict)

# Load unique products
data_files = os.listdir("data")
product_list = []
for f in data_files:
    temp_df = pd.read_csv(os.path.join("data", f))
    temp_df["category"] = f.split(".")[0]
    temp_df = temp_df[["name", "category"]].drop_duplicates()
    product_list.append(temp_df)

products_df = pd.concat(product_list)
del(product_list, temp_df, f, data_files)



# Search products in the database
products_dict = products_df.to_dict(orient="records")

results_list = []
for prod in products_dict:
    hits = qdrant.search(
        collection_name="sanctions",
        query_vector=encoder.encode(prod["name"]).tolist(),
        limit=1,
    )
    new_prod = {
        **prod,
        "hscode": hits[0].payload.get("code"),
        "sanctions": hits[0].payload.get("sanctions"),
        "confidence": round(hits[0].score, 4)
    }
    results_list.append(new_prod)
    

products_df = pd.DataFrame(results_list)

products_df.to_sql(name="products", con=con_cpi, if_exists="replace", index=False)
