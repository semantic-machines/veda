import WebSocket from 'isomorphic-ws';

import nodeFetch, {
  Headers,
  Request,
  Response,
} from 'node-fetch';
import makeFetchCookie from 'fetch-cookie';
import {CookieJar} from 'tough-cookie';

// Create cookie jar and wrap fetch for automatic cookie handling
let cookieJar = new CookieJar();
let fetchWithCookies = makeFetchCookie(nodeFetch, cookieJar);

// Export function to reset cookies by creating new jar (needed when switching users in tests)
globalThis.resetCookieJar = () => {
  cookieJar = new CookieJar();
  fetchWithCookies = makeFetchCookie(nodeFetch, cookieJar);
  globalThis.fetch = fetchWithCookies;
};

// Always override fetch with cookie-enabled version for tests
globalThis.fetch = fetchWithCookies;
globalThis.Headers = Headers;
globalThis.Request = Request;
globalThis.Response = Response;

if (!globalThis.location) {
  globalThis.location = {origin: 'http://localhost:8080'};
}

if (!globalThis.WebSocket) {
  globalThis.WebSocket = WebSocket;
}
