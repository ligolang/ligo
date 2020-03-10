import fetch from 'node-fetch';

const URL = 'https://api.tez.ie/keys/carthagenet/';
const AUTHORIZATION_HEADER = 'Bearer ligo-ide';

export async function fetchRandomPrivateKey(): Promise<string> {
  const response = await fetch(URL, {
    method: 'POST',
    headers: { Authorization: AUTHORIZATION_HEADER }
  });

  return response.text();
}
