# NCAAWomensSoccerData
A repository of women's soccer data scraped from stats.ncaa.org, plus the scrapers that did the work.

## Updating the data with Python

### Setup

Requires Python 3.12+ and [uv](https://docs.astral.sh/uv/).

```bash
uv sync
uv run playwright install chromium
```

### Women's soccer

The scripts read team URLs from `url_csvs/ncaa_womens_soccer_teamurls_{season}.csv` and write CSVs to `data/`. The first argument is the season; it defaults to `2026` if omitted.

```bash
# Match stats -> data/ncaa_womens_soccer_matchstats_{season}.csv
uv run NCAAWomensSoccerMatchScraper.py 2026

# Optional second argument limits the number of teams, useful for a test run
uv run NCAAWomensSoccerMatchScraper.py 2026 10

# Player stats -> data/ncaa_womens_soccer_playerstats_{season}.csv
uv run NCAAWomensSoccerPlayerScraper.py 2026

# Reports -> data/ncaa_womens_soccer_{attendance,toughest_schedule,non_conference,
#   non_conference_institution,attendance_sg_highs,wlt_streaks,overtimes}_{season}.csv
uv run NCAAWomensSoccerReportScraper.py 2026
```

### Men's soccer

The men's scrapers work the same way, reading `url_csvs/ncaa_mens_soccer_teamurls_{season}.csv` and writing to `data/ncaa_mens_soccer_*`.

```bash
uv run NCAAMensSoccerMatchScraper.py 2026
uv run NCAAMensSoccerPlayerScraper.py 2026
uv run NCAAMensSoccerReportScraper.py 2026

# Combine per-season match files into data/ncaa_mens_soccer_matchstats_2018_2025.csv
uv run combine_mens_matchstats.py
```

### Notes

- The scrapers launch a visible Chromium window (`headless=False`) and pause briefly between requests.
- Match and player scrapers require the corresponding `url_csvs/ncaa_*_soccer_teamurls_{season}.csv` file to exist first. Women's team URL files can be regenerated with `uv run build_womens_soccer_urls.py` (or `r_scripts/LinkHarvesterScraper.R`); the build script requires `ncaa_stats_py`, installable via `uv add git+https://github.com/Sports-Roster-Data/ncaa_stats_py.git`.
- The report scraper derives the season id from the team URL CSV and scrapes all available reports for that season. 
