#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "telethon",
# ]
# ///
"""Upload a profile photo to Telegram."""

import asyncio
import os
import sys
from pathlib import Path

from telethon import TelegramClient
from telethon.tl.functions.photos import UploadProfilePhotoRequest

DATAROOM_CONFIG = Path("/Users/v/Code_2026/dataroom/collect/telegram/config")
SESSION_FILE = str(DATAROOM_CONFIG / "session")
API_ID = os.environ.get("TELEGRAM_API_ID", "")
API_HASH = os.environ.get("TELEGRAM_API_HASH", "")


async def main():
    photo_path = sys.argv[1] if len(sys.argv) > 1 else "/Users/v/Code_2026/risk/avatar.png"

    client = TelegramClient(SESSION_FILE, int(API_ID), API_HASH)
    await client.connect()
    if not await client.is_user_authorized():
        print("Not authenticated")
        sys.exit(1)

    file = await client.upload_file(photo_path)
    await client(UploadProfilePhotoRequest(file=file))
    print(f"Profile photo updated with {photo_path}")
    await client.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
