'use client';
//React Imports
import React, { useMemo, ReactNode } from 'react';

//Mui imports
import { ThemeProvider as MUIThemeProvider } from '@mui/material/styles';
import CssBaseline from '@mui/material/CssBaseline';

//Local file
import { getTheme } from './theme';


export const ThemeModeProvider = ({ children }: { children: ReactNode }) => {
    const theme = useMemo(() => getTheme('light'), []);
  
    return (
        <MUIThemeProvider theme={theme}>
          {/* CssBaseline kickstart an elegant, consistent, and simple baseline to build upon. */}
          <CssBaseline />
          {children}
        </MUIThemeProvider>
    );
  };