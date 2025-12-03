import { NextRequest, NextResponse } from 'next/server';

const allowedUserRoutes = new Set(['alice', 'bob', 'connected-wallet']);

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;

  if (
    pathname.startsWith('/_next') ||
    pathname.startsWith('/api') ||
    pathname.startsWith('/assets') ||
    pathname === '/favicon.ico'
  ) {
    return NextResponse.next();
  }

  if (pathname === '/' || pathname === '/mint-authority') {
    return NextResponse.redirect(new URL('/connected-wallet', request.url));
  }

  const segments = pathname.split('/').filter(Boolean);
  if (segments.length > 0 && allowedUserRoutes.has(segments[0])) {
    return NextResponse.next();
  }

  return NextResponse.redirect(new URL('/connected-wallet', request.url));
}

export const config = {
  matcher: ['/((?!_next/static|_next/image|favicon.ico|api).*)'],
};
