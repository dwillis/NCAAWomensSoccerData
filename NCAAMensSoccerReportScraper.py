# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "pandas",
#     "playwright",
# ]
# ///

import sys
import time
import re
import pandas as pd
from urllib.parse import parse_qs, urlparse
from playwright.sync_api import sync_playwright


ROOT_URL = "https://stats.ncaa.org"


def clean_value(val):
    """Strip trailing slashes and whitespace."""
    if val is None:
        return ""
    val = val.strip()
    if val.endswith("/"):
        val = val[:-1].strip()
    return val


def get_season_id(season):
    """Extract season_id from the team URL CSV."""
    urls_df = pd.read_csv(f"url_csvs/ncaa_mens_soccer_teamurls_{season}.csv")
    first_url = urls_df.iloc[0, 2]
    parsed = urlparse(first_url)
    params = parse_qs(parsed.query)
    return params["game_sport_year_ctl_id"][0]


def discover_report_urls(page, season_id):
    """Navigate to the reports index and extract all report URLs."""
    index_url = f"{ROOT_URL}/reports/index?division=1.0&id={season_id}"
    page.goto(index_url, timeout=30000)
    time.sleep(2)

    reports = {}
    links = page.query_selector_all("a")
    for a in links:
        text = a.inner_text().strip()
        href = a.get_attribute("href") or ""
        if not href or href == "#":
            continue

        if "/reports/attendance?" in href:
            reports["attendance"] = ROOT_URL + href
        elif "/reports/toughest_schedule?" in href:
            reports["toughest_schedule"] = ROOT_URL + href
        elif "/reports/non_conference?" in href:
            reports["non_conference"] = ROOT_URL + href
            reports["non_conference_institution"] = ROOT_URL + href.replace(
                "non_conference?", "non_conference_institution?"
            )
        elif "attendance_sg_highs" in href:
            reports["attendance_sg_highs"] = ROOT_URL + href
        elif "wl_streaks" in href:
            reports["wlt_streaks"] = ROOT_URL + href
        elif "extra_periods" in href:
            reports["overtimes"] = ROOT_URL + href

    return reports


def parse_flat_table(page, table_selector="#stat_grid", skip_title_row=True):
    """Parse a table with a single header row into a DataFrame."""
    table = page.query_selector(table_selector)
    if not table:
        # Fallback: get the last table on the page
        tables = page.query_selector_all("table")
        if not tables:
            return None
        table = tables[-1]

    rows = table.query_selector_all("tr")
    if not rows:
        return None

    # Find header row (first row with th elements and no td)
    headers = []
    data_start = 0
    for i, row in enumerate(rows):
        th_cells = row.query_selector_all("th")
        td_cells = row.query_selector_all("td")
        if len(th_cells) > 0 and len(td_cells) == 0:
            headers = [clean_value(c.inner_text()) for c in th_cells]
            data_start = i + 1
            break
        elif skip_title_row and i == 0:
            # Skip title row (single cell spanning full width)
            continue

    if not headers:
        return None

    data = []
    for row in rows[data_start:]:
        cells = row.query_selector_all("td")
        if len(cells) != len(headers):
            continue
        values = [clean_value(c.inner_text()) for c in cells]
        # Skip empty/no-data rows
        if all(v == "" for v in values) or "No data available" in " ".join(values):
            continue
        data.append(dict(zip(headers, values)))

    return pd.DataFrame(data) if data else None


# --- Individual report parsers ---


def parse_attendance(page, url):
    """Attendance report: flat table."""
    page.goto(url, timeout=30000)
    time.sleep(2)
    df = parse_flat_table(page)
    if df is not None:
        col_map = {
            "Rank": "rank",
            "Institution": "institution",
            "Conference": "conference",
            "Accum Attendance": "accum_attendance",
            "Avg Attendance": "avg_attendance",
            "Stadium Capacity": "stadium_capacity",
            "Pct Capacity": "pct_capacity",
            "Games": "games",
        }
        df = df.rename(columns=col_map)
    return df


def parse_toughest_schedule(page, url):
    """Toughest Schedule: two header rows, flatten with prefixes."""
    page.goto(url, timeout=30000)
    time.sleep(2)

    table = page.query_selector("#stat_grid")
    if not table:
        return None

    rows = table.query_selector_all("tr")

    # Row 0: title (skip)
    # Row 1: group headers (Rank, Institution, Conference, Past Opposition, Future Opposition, Cumulative Opposition)
    # Row 2: sub-headers (Wins, Losses, Ties, Pct repeated 3x)
    # Row 3+: data

    flat_headers = [
        "rank", "institution", "conference",
        "past_wins", "past_losses", "past_ties", "past_pct",
        "future_wins", "future_losses", "future_ties", "future_pct",
        "cumulative_wins", "cumulative_losses", "cumulative_ties", "cumulative_pct",
    ]

    data = []
    for row in rows[3:]:
        cells = row.query_selector_all("td")
        if len(cells) != len(flat_headers):
            continue
        values = [clean_value(c.inner_text()) for c in cells]
        if all(v == "" for v in values):
            continue
        data.append(dict(zip(flat_headers, values)))

    return pd.DataFrame(data) if data else None


def parse_non_conference(page, url):
    """Non-Conference report: flat table (works for both conference and institution views)."""
    page.goto(url, timeout=30000)
    time.sleep(2)
    df = parse_flat_table(page)
    if df is not None:
        col_map = {
            "Conference": "conference",
            "Institution": "institution",
            "All": "all",
            "I": "division_i",
            "II": "division_ii",
            "III": "division_iii",
            "Non-Member": "non_member",
        }
        df = df.rename(columns=col_map)
    return df


def parse_attendance_sg_highs(page, url):
    """Attendance Single Game Highs: game-level table with two team columns."""
    page.goto(url, timeout=30000)
    time.sleep(2)

    tables = page.query_selector_all("table")
    if not tables:
        return None
    table = tables[-1]

    rows = table.query_selector_all("tr")
    if not rows:
        return None

    # Row 0 is header: Game Date, Teams (spanning), Score, Location, Attendance
    # Data rows have: date, team1, vs./@, team2, score, location, attendance
    headers = ["date", "team1", "indicator", "team2", "score", "location", "attendance"]

    data = []
    for row in rows[1:]:
        cells = row.query_selector_all("td")
        if len(cells) != len(headers):
            continue
        values = [clean_value(c.inner_text()) for c in cells]
        if all(v == "" for v in values):
            continue
        data.append(dict(zip(headers, values)))

    return pd.DataFrame(data) if data else None


def parse_wlt_streaks(page, url):
    """WLT Streaks: two header rows, flatten with category prefixes."""
    page.goto(url, timeout=30000)
    time.sleep(2)

    tables = page.query_selector_all("table")
    if not tables:
        return None
    table = tables[-1]

    rows = table.query_selector_all("tr")

    # Row 0: group headers (Rank, Team, Conference, Overall, Conference, Home, Road, Neutral Streak, Non-Division)
    # Row 1: sub-headers (WLT, Pct, Strk repeated 6x)
    # Row 2+: data

    flat_headers = [
        "rank", "team", "conference",
        "overall_wlt", "overall_pct", "overall_strk",
        "conference_wlt", "conference_pct", "conference_strk",
        "home_wlt", "home_pct", "home_strk",
        "road_wlt", "road_pct", "road_strk",
        "neutral_wlt", "neutral_pct", "neutral_strk",
        "non_division_wlt", "non_division_pct", "non_division_strk",
    ]

    data = []
    for row in rows[2:]:
        cells = row.query_selector_all("td")
        if len(cells) != len(flat_headers):
            continue
        values = [clean_value(c.inner_text()) for c in cells]
        if all(v == "" for v in values):
            continue
        data.append(dict(zip(flat_headers, values)))

    return pd.DataFrame(data) if data else None


def parse_overtimes(page, url):
    """Overtimes/Extra Periods: game-level table."""
    page.goto(url, timeout=30000)
    time.sleep(2)

    tables = page.query_selector_all("table")
    if not tables:
        return None
    table = tables[-1]

    rows = table.query_selector_all("tr")
    if not rows:
        return None

    # Header row has: Game Date, Teams (spanning), Score, Location, Attendance, Extra Periods
    # Data: date, team1, vs./@, team2, score, location, attendance, extra_periods
    headers = [
        "date", "team1", "indicator", "team2", "score",
        "location", "attendance", "extra_periods",
    ]

    data = []
    for row in rows[1:]:
        cells = row.query_selector_all("td")
        if len(cells) != len(headers):
            continue
        values = [clean_value(c.inner_text()) for c in cells]
        if all(v == "" for v in values):
            continue
        data.append(dict(zip(headers, values)))

    return pd.DataFrame(data) if data else None


# --- Report registry ---

REPORT_PARSERS = {
    "attendance": parse_attendance,
    "toughest_schedule": parse_toughest_schedule,
    "non_conference": parse_non_conference,
    "non_conference_institution": parse_non_conference,
    "attendance_sg_highs": parse_attendance_sg_highs,
    "wlt_streaks": parse_wlt_streaks,
    "overtimes": parse_overtimes,
}


def main():
    season = sys.argv[1] if len(sys.argv) > 1 else "2025"
    output_prefix = f"data/ncaa_mens_soccer"

    print(f"Deriving season_id from team URL CSV for {season}...")
    season_id = get_season_id(season)
    print(f"Season ID: {season_id}")

    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=False,
            args=["--disable-blink-features=AutomationControlled"],
        )
        context = browser.new_context(
            user_agent=(
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/120.0.0.0 Safari/537.36"
            )
        )
        page = context.new_page()

        print("Discovering report URLs...")
        report_urls = discover_report_urls(page, season_id)
        print(f"Found {len(report_urls)} reports: {', '.join(report_urls.keys())}")

        for name, url in report_urls.items():
            print(f"\nScraping {name}...")
            parser = REPORT_PARSERS.get(name)
            if not parser:
                print(f"  No parser for {name}, skipping")
                continue

            try:
                df = parser(page, url)
                if df is not None and not df.empty:
                    output_file = f"{output_prefix}_{name}_{season}.csv"
                    df.to_csv(output_file, index=False)
                    print(f"  Wrote {len(df)} rows to {output_file}")
                else:
                    print(f"  No data returned")
            except Exception as e:
                print(f"  Error: {e}")

            time.sleep(1)

        browser.close()

    print("\nDone!")


if __name__ == "__main__":
    main()
