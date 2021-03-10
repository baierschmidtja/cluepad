const CACHE = "cluepad-cache2";
const PREVIOUS_CACHE = "cluepad-cache";

self.addEventListener("message", (event) => {
  if (event.data && event.data.type === "SKIP_WAITING") {
    self.skipWaiting();
  }
});

self.addEventListener('install', async (event) => {
  event.waitUntil(
    caches.open(CACHE)
      .then((cache) => cache.addAll(
        [
          'https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css',
          '/fonts/ReenieBeanie.woff2',
          '/images/texture-seamless-wood-4.jpg',
          '/images/yellow-notepad-long.jpg',
          '/images/yellow-notepad-wire.jpg',
          '/cluepad.css',
          '/cluepad.js',
          '/index.html'
        ]
      )
    )
  );
});

self.addEventListener('fetch', function(event) {
  event.respondWith(
    caches.match(event.request).then(function(response) {
      return response || fetch(event.request);
    })
  );
});

self.addEventListener('activate', function (event) {
  event.waitUntil(
    caches.keys().then(function (cacheNames) {
      return Promise.all(
        cacheNames
          .filter(function (cacheName) {
            if (cacheName === PREVIOUS_CACHE) {
              return true;
            }
          })
          .map(function (cacheName) {
            return caches.delete(cacheName);
          }),
      );
    }),
  );
});
