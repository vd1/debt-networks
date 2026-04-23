import { chromium } from 'playwright';

const url = process.argv[2] || 'https://hackmd.io/join/note/lHc0bSXq0s';
const headless = process.env.HEADLESS !== '0';

const browser = await chromium.launch({ headless });
const context = await browser.newContext({
  userAgent: 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
  viewport: { width: 1280, height: 900 },
  locale: 'en-US',
});
const page = await context.newPage();
await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 60000 });

// Wait for rendered content — give the WAF challenge up to 2 min to clear (manual if needed)
await page.waitForSelector('.markdown-body, #doc, .view-area', { timeout: 120000 }).catch(() => {});
await page.waitForLoadState('networkidle', { timeout: 30000 }).catch(() => {});

const content = await page.evaluate(() => {
  const el = document.querySelector('.markdown-body') || document.querySelector('#doc') || document.querySelector('.view-area');
  return el ? el.innerText : document.body.innerText;
});

console.log(content);
await browser.close();
