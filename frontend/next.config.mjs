/** @type {import('next').NextConfig} */
const nextConfig =
  { async rewrites() {
    return [
      {
        source: '/api/v1/:path*',
        destination: 'http://localhost:8080/:path*' // Proxy to Backend
      }
    ]
  },
  output: 'export'
  };

export default nextConfig;
