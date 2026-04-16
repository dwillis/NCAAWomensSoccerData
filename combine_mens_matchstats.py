import glob
import pandas as pd

files = sorted(glob.glob("data/ncaa_mens_soccer_matchstats_2[0-9]*.csv"))
df = pd.concat((pd.read_csv(f) for f in files), ignore_index=True)
df.to_csv("data/ncaa_mens_soccer_matchstats_2018_2025.csv", index=False)
print(f"Combined {len(files)} files into {len(df)} rows.")
