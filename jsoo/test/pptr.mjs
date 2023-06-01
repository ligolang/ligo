import puppeteer from "puppeteer";
import { blue, cyan, green, magenta, red, yellow } from "colorette";
import StaticServer from "static-server";

const { JSOO_DIST_DIR } = process.env;
if (!JSOO_DIST_DIR) {
  throw new Error("JSOO_DIST_DIR must be set in the environment");
}
const PORT = 1337;
const JSOO_TEST_PAGE = `http://localhost:${PORT}`;

function serveLigoJS(rootPath, port) {
  return new Promise(function (resolve, reject) {
    var server = new StaticServer({
      rootPath,
      port,
      name: "my-http-server", // optional, will set "X-Powered-by" HTTP header
      host: "localhost", // optional, defaults to any interface
      cors: "*", // optional, defaults to undefined
      followSymlink: true, // optional, defaults to a 404 error
    });
    server.start(() =>
      resolve(() => {
        server.stop();
      })
    );
  });
}

const log = console.log;

async function main() {
  log("Serving ligo assets from", JSOO_DIST_DIR);
  let stopServer = await serveLigoJS(JSOO_DIST_DIR, PORT);
  log("Listening on port", PORT);
  log("Starting puppeteer");
  const browser = await puppeteer.launch({
    ignoreDefaultArgs: ["--disable-extensions"],
    devtools: false,
    pipe: true,
    product: "firefox",
    headless: "new",
  });
  log("Launched puppeteer. Going to page:", JSOO_TEST_PAGE);
  const page = await browser.newPage();
  page
    .on("console", (message) => {
      const type = message.type().substr(0, 3).toUpperCase();
      const colors = {
        LOG: (text) => text,
        ERR: red,
        WAR: yellow,
        INF: cyan,
      };
      const color = colors[type] || blue;
      console.log(color(`${message.text()}`));
    })
    .on("pageerror", ({ message }) => {
      throw new Error(message);
    });

  await page.goto(JSOO_TEST_PAGE, {
    timeout: 2 * 60 * 1000,
    waitUntil: "load",
  });
  log("Page loaded with no errors");
  log("Closing puppeteer");
  await browser.close();
  log("Shutting down static server");
  stopServer();
}

main()
  .then(() => {})
  .catch((e) => {
    console.log(">>> ERROR <<<");
    console.log(e.message);
    console.log(e);
    process.exit(-1);
  });
