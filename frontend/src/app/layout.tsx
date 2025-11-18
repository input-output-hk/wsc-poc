//NextJS Imports
import type { Metadata } from "next";

//Local files
import "./styles/globals.css";
import ClientLayout from "./clientLayout";
import { buildInitialWalletSnapshot, loadDemoEnvironment } from "./lib/demoEnvironment.server";

export const metadata: Metadata = {
  title: "WST - Programmable token demonstration",
  description: "Created by the djed team at IOG",
};

export default async function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  const { environment } = await loadDemoEnvironment();
  const initialWallets = await buildInitialWalletSnapshot(environment);

  return (
    <html lang="en">
      <head>
        <link rel="preconnect" href="https://fonts.googleapis.com" />
        <link rel="preconnect" href="https://fonts.gstatic.com" />
        <link href="https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&display=swap" rel="stylesheet" />
      </head>
      <body>
        <ClientLayout initialDemoEnvironment={environment} initialWallets={initialWallets}>
          {children}
        </ClientLayout>
      </body>
    </html>
  );
}
