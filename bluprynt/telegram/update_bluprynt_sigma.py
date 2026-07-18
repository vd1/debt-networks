#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "telethon>=1.43.2,<2",
# ]
# ///
"""Update the local Bluprynt / Sigma Telegram archive.

The raw archive is local-only by default. See ``bp/telegram/.gitignore``.
Credentials and the Telethon session are reused from the sibling dataroom repo.
"""

from __future__ import annotations

import argparse
import asyncio
import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    from telethon import TelegramClient
except ImportError:
    print("Missing dependency: telethon. Run with `uv run bp/telegram/update_bluprynt_sigma.py`.", file=sys.stderr)
    sys.exit(1)


SCRIPT_DIR = Path(__file__).resolve().parent
DOWNLOADS_DIR = SCRIPT_DIR / "downloads"
ARCHIVE_PATH = DOWNLOADS_DIR / "bluprynt_sigma_labs.jsonl"
LOG_PATH = DOWNLOADS_DIR / "bluprynt_sigma_labs.log"
STATE_PATH = DOWNLOADS_DIR / "state.json"

DEFAULT_DATAROOM = Path("/Users/v/Code_2026/dataroom")
TARGET = "Bluprynt - Sigma Labs"


def load_env(path: Path) -> dict[str, str]:
    env: dict[str, str] = {}
    if not path.is_file():
        return env
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        if line.startswith("export "):
            line = line.removeprefix("export ").strip()
        key, value = line.split("=", 1)
        env[key.strip()] = value.strip().strip("\"'")
    return env


def sender_label(sender: Any) -> str | None:
    if sender is None:
        return None
    parts = [getattr(sender, "first_name", None), getattr(sender, "last_name", None)]
    name = " ".join(part for part in parts if part).strip()
    return name or getattr(sender, "title", None) or getattr(sender, "username", None)


def read_archive(path: Path) -> dict[tuple[int, int], dict[str, Any]]:
    records: dict[tuple[int, int], dict[str, Any]] = {}
    if not path.is_file():
        return records
    for raw in path.read_text(encoding="utf-8").splitlines():
        if not raw.strip():
            continue
        try:
            record = json.loads(raw)
        except json.JSONDecodeError:
            continue
        chat_id = record.get("chat_id")
        msg_id = record.get("msg_id")
        if isinstance(chat_id, int) and isinstance(msg_id, int):
            records[(chat_id, msg_id)] = record
    return records


def record_key(record: dict[str, Any]) -> tuple[str, int]:
    return (str(record.get("date") or ""), int(record.get("msg_id") or 0))


def write_archive(records: list[dict[str, Any]]) -> None:
    DOWNLOADS_DIR.mkdir(parents=True, exist_ok=True)
    text = "\n".join(json.dumps(record, ensure_ascii=False) for record in records)
    ARCHIVE_PATH.write_text(text + ("\n" if text else ""), encoding="utf-8")


def write_log(records: list[dict[str, Any]]) -> None:
    lines: list[str] = []
    for record in records:
        lines.append("=" * 72)
        who = record.get("sender_name") or ("me" if record.get("out") else "unknown")
        if record.get("out") and record.get("sender_name"):
            who = f"{who} (you)"
        lines.append(f"{record.get('date')} | {who} | msg {record.get('msg_id')}")
        if record.get("media"):
            lines.append("[media attached]")
        if record.get("reply_to_msg_id"):
            lines.append(f"[reply to {record.get('reply_to_msg_id')}]")
        lines.append(str(record.get("text") or ""))
        lines.append("")
    LOG_PATH.write_text("\n".join(lines), encoding="utf-8")


def write_state(records: list[dict[str, Any]], *, fetched: int, changed: int, dataroom: Path) -> None:
    state = {
        "target": TARGET,
        "archive": str(ARCHIVE_PATH.relative_to(SCRIPT_DIR)),
        "message_count": len(records),
        "first_date": records[0]["date"] if records else None,
        "last_date": records[-1]["date"] if records else None,
        "max_msg_id": max((int(record["msg_id"]) for record in records), default=None),
        "last_fetch_count": fetched,
        "last_changed_count": changed,
        "updated_at": datetime.now(timezone.utc).isoformat(),
        "dataroom": str(dataroom),
    }
    STATE_PATH.write_text(json.dumps(state, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")


async def find_dialog(client: TelegramClient) -> Any:
    nearby: list[str] = []
    async for dialog in client.iter_dialogs():
        name = dialog.name or ""
        low = name.casefold()
        if TARGET.casefold() == low or TARGET.casefold() in low or ("bluprynt" in low and "sigma" in low):
            return dialog
        if "bluprynt" in low or "sigma" in low:
            nearby.append(name)
    if nearby:
        print("No exact matching dialog found. Nearby candidates:", file=sys.stderr)
        for name in nearby:
            print(f"- {name}", file=sys.stderr)
    raise SystemExit(2)


async def fetch_records(
    *,
    dataroom: Path,
    limit: int,
    tail_slack: int,
    full: bool,
    existing: dict[tuple[int, int], dict[str, Any]],
) -> tuple[list[dict[str, Any]], Any]:
    config = dataroom / "collect" / "telegram" / "config"
    env = load_env(config / "env")
    api_id = os.environ.get("TELEGRAM_API_ID") or env.get("TELEGRAM_API_ID")
    api_hash = os.environ.get("TELEGRAM_API_HASH") or env.get("TELEGRAM_API_HASH")
    if not api_id or not api_hash:
        raise SystemExit("Missing TELEGRAM_API_ID/TELEGRAM_API_HASH in dataroom env")

    client = TelegramClient(str(config / "session"), int(api_id), api_hash)
    await client.start()
    dialog = await find_dialog(client)

    max_existing_id = max((msg_id for chat_id, msg_id in existing if chat_id == dialog.id), default=0)
    min_id = 0 if full or max_existing_id <= 0 else max(0, max_existing_id - tail_slack)

    records: list[dict[str, Any]] = []
    async for msg in client.iter_messages(dialog.entity, limit=limit, min_id=min_id):
        sender_name = None
        try:
            sender_name = sender_label(await msg.get_sender())
        except Exception:
            pass
        date = msg.date
        if date.tzinfo is None:
            date = date.replace(tzinfo=timezone.utc)
        records.append({
            "chat": dialog.name or str(dialog.id),
            "chat_id": dialog.id,
            "msg_id": msg.id,
            "date": date.isoformat(),
            "out": bool(getattr(msg, "out", False)),
            "sender_name": sender_name,
            "text": msg.text or "",
            "media": bool(msg.media),
            "reply_to_msg_id": getattr(msg.reply_to, "reply_to_msg_id", None) if msg.reply_to else None,
        })

    await client.disconnect()
    return records, dialog


async def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dataroom", type=Path, default=Path(os.environ.get("DATAROOM_DIR", DEFAULT_DATAROOM)))
    parser.add_argument("--limit", type=int, default=int(os.environ.get("BP_TG_LIMIT", "1000")))
    parser.add_argument("--tail-slack", type=int, default=int(os.environ.get("BP_TG_TAIL_SLACK", "50")))
    parser.add_argument("--full", action="store_true", help="Fetch from the start of the configured limit instead of only the recent tail.")
    args = parser.parse_args()

    existing = read_archive(ARCHIVE_PATH)
    fetched, dialog = await fetch_records(
        dataroom=args.dataroom.expanduser().resolve(),
        limit=args.limit,
        tail_slack=args.tail_slack,
        full=args.full,
        existing=existing,
    )

    changed = 0
    for record in fetched:
        key = (int(record["chat_id"]), int(record["msg_id"]))
        if existing.get(key) != record:
            changed += 1
        existing[key] = record

    records = sorted(existing.values(), key=record_key)
    write_archive(records)
    write_log(records)
    write_state(records, fetched=len(fetched), changed=changed, dataroom=args.dataroom)

    print(f"Dialog: {dialog.name} ({dialog.id})")
    print(f"Fetched: {len(fetched)}")
    print(f"Changed/new: {changed}")
    print(f"Archive messages: {len(records)}")
    print(f"Archive: {ARCHIVE_PATH}")
    print(f"Log: {LOG_PATH}")
    print(f"State: {STATE_PATH}")


if __name__ == "__main__":
    asyncio.run(main())
