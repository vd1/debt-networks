import { chromium } from 'playwright';

const url = process.argv[2] || 'https://hackmd.io/join/note/lHc0bSXq0s';
const browser = await chromium.launch({ headless: true });
const page = await browser.newPage();
await page.goto(url, { waitUntil: 'networkidle', timeout: 30000 });

// Wait for rendered content
await page.waitForSelector('.markdown-body, #doc, .view-area', { timeout: 15000 }).catch(() => {});

const content = await page.evaluate(() => {
  const el = document.querySelector('.markdown-body') || document.querySelector('#doc') || document.querySelector('.view-area');
  return el ? el.innerText : document.body.innerText;
});

console.log(content);
await browser.close();
