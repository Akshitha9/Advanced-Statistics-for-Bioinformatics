import sqlite3
import pandas as pd

df = pd.read_csv('data_clinical_patient.csv')
df1 = pd.read_csv('data_mrna_seq_v2_rsem_zscores_ref_all_samples.csv')

with sqlite3.connect('db/project.db') as con:
    df.to_sql("patients", con)

with sqlite3.connect('db/project.db') as con:
    df.to_sql("data_mrna_seq_v2_rsem_zscores_ref_all_samples", con)
    
