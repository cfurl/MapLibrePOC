const { chromium } = require("playwright");
const fs = require("fs");
const path = require("path");

// ============================================================
// CONFIG
// ============================================================

const WIDTH = 1920;
const HEIGHT = 1080;

const URL =
  "http://127.0.0.1:8000/poc/web/range_frame.html";

const OUTPUT_DIR =
  "C:\\stg4\\MapLibrePOC\\texas_COG_range\\output\\frame";

const OUTPUT_FILE =
  path.join(
    OUTPUT_DIR,
    "stage4_range_20260711_to_20260718xxx.png"
  );

// ============================================================
// CAPTURE
// ============================================================

(async () => {

  fs.mkdirSync(
    OUTPUT_DIR,
    {
      recursive: true
    }
  );

  const browser =
    await chromium.launch({
      headless: true
    });

  const page =
    await browser.newPage({
      viewport: {
        width: WIDTH,
        height: HEIGHT
      },
      deviceScaleFactor: 1
    });

  page.on(
    "console",
    msg => {
      console.log(
        "[browser]",
        msg.text()
      );
    }
  );

  page.on(
    "pageerror",
    error => {
      console.error(
        "[browser error]",
        error
      );
    }
  );

  console.log("");
  console.log("=======================================");
  console.log("Rendering range frame");
  console.log(URL);

  await page.goto(
    URL,
    {
      waitUntil: "domcontentloaded",
      timeout: 120000
    }
  );

  await page.waitForFunction(
    () =>
      window.__MOVIE_FRAME_READY__ === true,
    null,
    {
      timeout: 120000
    }
  );

  await page.screenshot({
    path: OUTPUT_FILE,
    type: "png"
  });

  console.log("");
  console.log("Saved:");
  console.log(OUTPUT_FILE);
  console.log("");
  console.log("RANGE FRAME COMPLETE");

  await browser.close();

})().catch(
  error => {

    console.error("");
    console.error("CAPTURE FAILED:");
    console.error(error);

    process.exit(1);
  }
);
