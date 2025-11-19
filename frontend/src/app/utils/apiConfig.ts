/**
 * Get the API base URL
 * - In development: returns empty string (uses Next.js rewrites)
 * - In production: returns NEXT_PUBLIC_API_URL for direct API calls
 */
export const getApiBaseUrl = (): string => {
  return process.env.NEXT_PUBLIC_API_URL || '';
};

/**
 * Build a full API endpoint URL
 * @param path - The API path (e.g., '/api/v1/query/address/...')
 * @returns Full URL for the API endpoint
 */
export const getApiUrl = (path: string): string => {
  const baseUrl = getApiBaseUrl();
  return `${baseUrl}${path}`;
};
