import Profile from '.';

export async function generateStaticParams() {
  return [
    { username: 'alice' },
    { username: 'bob' },
    { username: 'connected-wallet' } // connected wallet
  ]
}

export default function ProfilePage() {
  return <Profile />
}