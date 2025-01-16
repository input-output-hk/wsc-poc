import Profile from '.';

export async function generateStaticParams() {
  return [
    { username: 'user-a' },
    { username: 'user-b' },
    { username: 'connected-wallet' } // connected wallet
  ]
}

export default function ProfilePage() {
  return <Profile />
}