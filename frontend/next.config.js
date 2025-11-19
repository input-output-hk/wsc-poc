/** @type {import('next').NextConfig} */
const { PHASE_DEVELOPMENT_SERVER } = require('next/constants');
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');

/**
 * Webpack config used in all environments
 * @param {*} config 
 * @param {*} param1 
 * @returns 
 */
const webpackConfig = (config, { isServer }) => {
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
  
  return config;
};

module.exports = (phase, {defaultConfig}) => {
  if (phase === PHASE_DEVELOPMENT_SERVER) {
    return {
      /* NextJS development-only config options here */
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
        const apiUrl = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8080';
        return [
          {
            source: '/api/v1/:path*', // Match all routes starting with /api/v1/
            destination: `${apiUrl}/api/v1/:path*`, // Proxy to backend server
          },
        ];
      },
      async redirects() {
        return [
          {
            source: '/',
            destination: '/connected-wallet',
            permanent: true, // Use true for a 301 redirect, false for 302
          },
        ];
      },
      experimental: {
        esmExternals: true, // Ensure modern module support
      },
      webpack: webpackConfig
    }
  }
  // Default NextJS config for other phases (e.g., production, static export)
  return { 
    output: 'export',
    webpack: webpackConfig,
    experimental: {
      esmExternals: true, // Ensure modern module support
    },
  
    // https://github.com/Anastasia-Labs/lucid-evolution/issues/437
    serverExternalPackages: [
      "@lucid-evolution/lucid"
    ]
  }
}  
