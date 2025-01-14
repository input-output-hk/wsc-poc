const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');


  /** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  serverExternalPackages: [
    "@lucid-evolution/lucid",
  ],
  async headers() {
    return [
      {
        // matching all API routes
        source: "/api/v1/:path*",
        headers: [
          { key: "Access-Control-Allow-Credentials", value: "true" },
          { key: "Access-Control-Allow-Origin", value: "*" },
          { key: "Access-Control-Allow-Methods", value: "GET,OPTIONS,PATCH,DELETE,POST,PUT" },
          { key: "Access-Control-Allow-Headers", value: "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version" },
        ]
      }
    ]
  },
  async rewrites() {
    return [
      {
        source: '/api/v1/:path*', // Match all routes starting with /api/v1/
        destination: 'http://localhost:8080/api/v1/:path*', // Proxy to backend server
      },
    ];
  },
  async redirects() {
    return [
      {
        source: '/',
        destination: '/mint-authority',
        permanent: true, // Use true for a 301 redirect, false for 302
      },
    ];
  },
  experimental: {
    esmExternals: true, // Ensure modern module support
  },
  webpack: (config, { isServer }) => {
    // Ensure `resolve.plugins` exists
    config.resolve.plugins = [
      ...(config.resolve.plugins || []), // Keep existing plugins
      new TsconfigPathsPlugin({
        configFile: './tsconfig.json', // Adjust the path to your tsconfig.json if necessary
      }),
    ];

    config.experiments = {
      ...config.experiments,
      asyncWebAssembly: true, // Enable async WebAssembly
      topLevelAwait: true,
      layers: true
    };
    if (!isServer) {
      config.output.environment = { ...config.output.environment, asyncFunction: true };
    }

    // Add fallback and resolve configurations for browser compatibility
    config.resolve = {
      ...config.resolve,
      extensions: ['.ts', '.tsx', '.js', '.mjs'],
      fallback: {
        https: require.resolve('https-browserify'),
        http: require.resolve('stream-http'),
        'get-port-please': false,
        net: false,
        fs: false,
        os: false,
        path: false,
        events: require.resolve('events/'),
        buffer: require.resolve('buffer/'),
        stream: require.resolve('readable-stream'),
        crypto: require.resolve('crypto-browserify'),
        constants: require.resolve('constants-browserify'),
        zlib: require.resolve('browserify-zlib'),
        dns: false,
        tls: false,
        process: false,
        child_process: false,
      },
    };
    
    return config;
  },
};

module.exports = nextConfig;
  