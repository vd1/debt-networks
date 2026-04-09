#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "telethon",
# ]
# ///
"""Export all messages from a specific Telegram group chat to markdown."""

import asyncio
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

try:
    from telethon import TelegramClient
except ImportError:
    print("Run via: uv run export_tg_chat.py", file=sys.stderr)
    sys.exit(1)

# Reuse dataroom's config dir for session/credentials
DATAROOM_CONFIG = Path("/Users/v/Code_2026/dataroom/collect/telegram/config")
SESSION_FILE = str(DATAROOM_CONFIG / "session")

API_ID = os.environ.get("TELEGRAM_API_ID", "")
API_HASH = os.environ.get("TELEGRAM_API_HASH", "")

CHAT_NAME = "Vincent, Amaury, Daniel, Hamza"
OUTPUT = Path(__file__).parent / "tg_chat_export.md"


async def main():
    if not API_ID or not API_HASH:
        print("Set TELEGRAM_API_ID and TELEGRAM_API_HASH env vars.")
        print(f"  source {DATAROOM_CONFIG / 'env'}")
        sys.exit(1)

    client = TelegramClient(SESSION_FILE, int(API_ID), API_HASH)
    await client.connect()
    if not await client.is_user_authorized():
        print("Not authenticated. Run auth.py first interactively:")
        print(f"  source {DATAROOM_CONFIG / 'env'}")
        print(f"  uv run /Users/v/Code_2026/dataroom/collect/telegram/scripts/auth.py")
        await client.disconnect()
        sys.exit(1)

    # Find the chat by name
    target = None
    async for dialog in client.iter_dialogs():
        if dialog.name == CHAT_NAME:
            target = dialog
            break

    if not target:
        print(f"Chat '{CHAT_NAME}' not found. Available chats:")
        async for d in client.iter_dialogs(limit=30):
            print(f"  - {d.name}")
        await client.disconnect()
        sys.exit(1)

    print(f"Found chat: {target.name} (id={target.id})", file=sys.stderr)

    # Fetch all messages (oldest first)
    messages = []
    async for msg in client.iter_messages(target.id, limit=None, reverse=True):
        sender = None
        if msg.sender:
            sender = getattr(msg.sender, "first_name", None) or str(msg.sender_id)
        messages.append({
            "date": msg.date.strftime("%Y-%m-%d %H:%M"),
            "sender": sender or "Unknown",
            "text": msg.text or ("[media]" if msg.media else "[empty]"),
        })

    await client.disconnect()

    # Write as markdown
    lines = [f"# Telegram: {CHAT_NAME}", f"Exported {len(messages)} messages\n", "---\n"]
    current_date = None
    for m in messages:
        day = m["date"].split(" ")[0]
        if day != current_date:
            current_date = day
            lines.append(f"\n## {day}\n")
        lines.append(f"**{m['sender']}** ({m['date'].split(' ')[1]}): {m['text']}\n")

    OUTPUT.write_text("\n".join(lines))
    print(f"Exported {len(messages)} messages -> {OUTPUT}")


if __name__ == "__main__":
    asyncio.run(main())
