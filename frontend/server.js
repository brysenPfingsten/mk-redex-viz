// server.js
const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');

const app  = express();
const PORT = process.env.PORT || 3000;

// 1) Serve Vite’s build artifacts from dist/
app.use(express.static(path.join(__dirname, 'dist')));

// 2) Fallback to index.html for client-side routing:
//    any request that isn’t an asset or /api/* should return dist/index.html
app.get(/^(?!\/api\/).*/, (req, res) => {
  res.sendFile(path.join(__dirname, 'dist', 'index.html'));
});

// 3) Proxy API to your Racket server:
app.use('/api', createProxyMiddleware({
  target: 'http://racket-server:5000',
  changeOrigin: true,
}));

app.listen(PORT, () => {
  console.log(`Server listening on http://0.0.0.0:${PORT}`);
});
