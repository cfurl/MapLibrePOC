const { chromium } = require("playwright");
const fs = require("fs");
const path = require("path");


// ============================================================
// DAILY TEST FRAMES
// ============================================================

const dates = [
  "20260709",
  "20260710",
  "20260711",
  "20260712",
  "20260713",
  "20260714",
  "20260715",
  "20260716",
  "20260717",
  "20260718"
];


// ============================================================
// OUTPUT
// ============================================================

const outputDir =
  path.resolve(
    __dirname,
    "..",
    "movie_frames"
  );

fs.mkdirSync(
  outputDir,
  {
    recursive:true
  }
);


// ============================================================
// SCREENSHOT SIZE
// ============================================================
//
// 1920 x 1080 = normal HD video.
//
// Later we can easily change this to:
// 2560 x 1440
// or
// 3840 x 2160
//

const WIDTH  = 1920;
const HEIGHT = 1080;


// ============================================================
// CAPTURE
// ============================================================

(async () => {

  const browser =
    await chromium.launch({
      headless:true
    });


  const page =
    await browser.newPage({

      viewport:{
        width:WIDTH,
        height:HEIGHT
      },

      deviceScaleFactor:1

    });


  // Browser console output is very useful
  // if one frame fails to render.

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


  for (
    let i = 0;
    i < dates.length;
    i++
  ) {

    const date =
      dates[i];


    const url =
      `http://127.0.0.1:8000/poc/web/movie.html?date=${date}`;


    console.log("");
    console.log(
      "======================================="
    );

    console.log(
      `Rendering ${date}`
    );

    console.log(
      url
    );


    await page.goto(
      url,
      {
        waitUntil:"domcontentloaded",
        timeout:120000
      }
    );


    // movie.html sets this only AFTER
    // MapLibre reaches its idle state.

    await page.waitForFunction(

      () =>
        window.__MOVIE_FRAME_READY__ === true,

      null,

      {
        timeout:120000
      }

    );


    const frameNumber =
      String(i + 1)
      .padStart(
        3,
        "0"
      );


    const outputFile =
      path.join(
        outputDir,
        `frame_${frameNumber}.png`
      );


    await page.screenshot({

      path:
        outputFile,

      type:
        "png"

    });


    console.log(
      `Saved: ${outputFile}`
    );

  }


  await browser.close();


  console.log("");
  console.log(
    "======================================="
  );

  console.log(
    "ALL FRAMES COMPLETE"
  );

})();