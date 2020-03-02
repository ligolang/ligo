const commonUtils = require('./common-utils');

const API_HOST = commonUtils.API_HOST;

const runCommandAndGetOutputFor = commonUtils.runCommandAndGetOutputFor;
const clearText = commonUtils.clearText;

const COMMAND = 'deploy';
const COMMAND_ENDPOINT = 'deploy';

async function deploy() {
  return await runCommandAndGetOutputFor(COMMAND, COMMAND_ENDPOINT);
}

describe('Deploy contract', () => {
  beforeAll(() => jest.setTimeout(60000));

  beforeEach(async () => await page.goto(API_HOST));

  it('should deploy', async done => {
    expect(await deploy()).toContain('The contract was successfully deployed to the babylonnet test network.');

    done();
  });

  it('should fail to deploy contract with invalid storage', async done => {
    await page.click('#command-select');
    await page.click(`#deploy`);

    await page.click(`#storage`);
    await clearText(page.keyboard);
    await page.keyboard.type('asdf');

    expect(await deploy()).toContain('Error: ');

    done();
  });
});
