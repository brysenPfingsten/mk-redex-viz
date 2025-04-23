export default {
  server: {
    proxy: {
      '/api': {
        target: 'http://racket-server:5000',
        changeOrigin: true,
      }
    }
  }
}
