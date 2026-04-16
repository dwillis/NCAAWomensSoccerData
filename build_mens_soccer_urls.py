"""Build ncaa_mens_soccer_teamurls_{year}.csv files for 2018-2024.

Uses the github install of ncaa_stats_py (Sports-Roster-Data/ncaa_stats_py)
to retrieve D1 men's soccer teams, then derives each season's
game_sport_year_ctl_id by fetching one team's page, and writes the
same three-column format already used for 2025:
    team, playerstatsurl, matchstatsurl
"""
import importlib.util
import re
import sys
import types
from pathlib import Path

import pandas as pd

# The installed package has a broken import in __init__.py (missing
# ncaa_stats_py.helpers.football). Load the soccer + utls modules directly
# without executing the package __init__.
_PKG_DIR = Path(
    "/Users/dpwillis/code/NCAAWomensSoccerData/.venv/lib/python3.12/site-packages/ncaa_stats_py"
)
_pkg = types.ModuleType("ncaa_stats_py")
_pkg.__path__ = [str(_PKG_DIR)]
sys.modules["ncaa_stats_py"] = _pkg
for _name in ("utls", "soccer"):
    _spec = importlib.util.spec_from_file_location(
        f"ncaa_stats_py.{_name}", str(_PKG_DIR / f"{_name}.py")
    )
    _mod = importlib.util.module_from_spec(_spec)
    sys.modules[f"ncaa_stats_py.{_name}"] = _mod
    _spec.loader.exec_module(_mod)

from ncaa_stats_py.soccer import get_soccer_teams  # noqa: E402
from ncaa_stats_py.utls import _safe_get_webpage  # noqa: E402

OUT_DIR = Path("/Users/dpwillis/code/NCAAWomensSoccerData/url_csvs")
SEASONS = range(2020, 2025)


def ctl_for_team(team_id: int) -> int:
    """Scrape the season-specific game_sport_year_ctl_id from a team page."""
    resp = _safe_get_webpage(f"https://stats.ncaa.org/teams/{team_id}")
    m = re.search(r"game_sport_year_ctl_id[=:]?\s*(\d+)", resp.text)
    if not m:
        raise RuntimeError(f"No ctl found for team_id={team_id}")
    return int(m.group(1))


def build_season(season: int) -> None:
    teams = get_soccer_teams(season, 1, get_womens_soccer_data=False)
    teams = teams.dropna(subset=["school_id"]).copy()
    teams["school_id"] = teams["school_id"].astype(int)
    teams = teams.drop_duplicates(subset=["school_id"]).sort_values("school_name")

    ctl = ctl_for_team(int(teams.iloc[0]["team_id"]))
    print(f"{season}: {len(teams)} teams, ctl={ctl}")

    rows = []
    for _, row in teams.iterrows():
        org = int(row["school_id"])
        rows.append(
            {
                "team": row["school_name"],
                "playerstatsurl": f"https://stats.ncaa.org/team/{org}/stats/{ctl}",
                "matchstatsurl": (
                    "https://stats.ncaa.org/player/game_by_game?"
                    f"org_id={org}&game_sport_year_ctl_id={ctl}"
                    "&stats_player_seq=-100"
                ),
            }
        )
    out = OUT_DIR / f"ncaa_mens_soccer_teamurls_{season}.csv"
    pd.DataFrame(rows).to_csv(out, index=False)
    print(f"  wrote {out}")


if __name__ == "__main__":
    for s in SEASONS:
        try:
            build_season(s)
        except Exception as e:
            print(f"{s}: SKIPPED ({e})")
