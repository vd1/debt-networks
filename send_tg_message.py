#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "telethon",
# ]
# ///
"""Send a message to a Telegram group chat, optionally as a reply.

Usage:
  send_tg_message.py '<message>'                        # send standalone
  send_tg_message.py '<message>' --reply '<search text>' # reply to a message containing <search text>
  send_tg_message.py '<message>' --reply-id <msg_id>     # reply to a specific message ID
"""

import asyncio
import os
import sys
from pathlib import Path

from telethon import TelegramClient

DATAROOM_CONFIG = Path("/Users/v/Code_2026/dataroom/collect/telegram/config")
SESSION_FILE = str(DATAROOM_CONFIG / "session")
API_ID = os.environ.get("TELEGRAM_API_ID", "")
API_HASH = os.environ.get("TELEGRAM_API_HASH", "")

CHAT_NAME = "Vincent, Amaury, Daniel, Hamza"


def parse_args():
    args = sys.argv[1:]
    if not args:
        print(__doc__)
        sys.exit(1)

    message = args[0]
    reply_search = None
    reply_id = None

    i = 1
    while i < len(args):
        if args[i] == "--reply" and i + 1 < len(args):
            reply_search = args[i + 1]
            i += 2
        elif args[i] == "--reply-id" and i + 1 < len(args):
            reply_id = int(args[i + 1])
            i += 2
        else:
            # Legacy: second positional arg = reply search text
            reply_search = args[i]
            i += 1

    return message, reply_search, reply_id


async def main():
    if not API_ID or not API_HASH:
        print("Set TELEGRAM_API_ID and TELEGRAM_API_HASH")
        sys.exit(1)

    message, reply_search, reply_id = parse_args()

    client = TelegramClient(SESSION_FILE, int(API_ID), API_HASH)
    await client.connect()
    if not await client.is_user_authorized():
        print("Not authenticated. Run auth.py first.")
        sys.exit(1)

    target = None
    async for dialog in client.iter_dialogs():
        if dialog.name == CHAT_NAME:
            target = dialog
            break

    if not target:
        print(f"Chat '{CHAT_NAME}' not found")
        sys.exit(1)

    # Find message to reply to
    if reply_search and not reply_id:
        async for msg in client.iter_messages(target.id, limit=100):
            if msg.text and reply_search in msg.text:
                reply_id = msg.id
                sender = getattr(msg.sender, "first_name", "?") if msg.sender else "?"
                preview = msg.text[:80].replace("\n", " ")
                print(f"Replying to [{sender}]: {preview}...")
                break
        if not reply_id:
            print(f"No message found containing '{reply_search}'. Sending standalone.")

    await client.send_message(target.id, message, reply_to=reply_id)
    print(f"Message sent to '{CHAT_NAME}'" + (f" (reply to {reply_id})" if reply_id else ""))
    await client.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
