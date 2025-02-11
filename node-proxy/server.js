const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');

const app = express();
const PORT = 3000;

app.use(express.static(path.join(__dirname, 'public')))

app.use('/api', createProxyMiddleware({
    target: 'http://racket-server:5000',
    changeOrigin: true,
}));

app.listen(PORT, () => {
    console.log(`Node.js proxy is running on http://localhost:${PORT}`)
});