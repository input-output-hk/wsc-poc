'use client';
//React Imports
import React, { createContext, useContext, useMemo, useState, ReactNode } from 'react';

//Mui imports
import { ThemeProvider as MUIThemeProvider } from '@mui/material/styles';
import CssBaseline from '@mui/material/CssBaseline';

//Local file
import { getTheme } from './theme';

type ThemeModeContextType = {
  toggleTheme: () => void;
  mode: 'light' | 'dark';
};

const ThemeModeContext = createContext<ThemeModeContextType>({ toggleTheme: () => {}, 
mode: 'light'
});


export const useThemeMode = () => useContext(ThemeModeContext);

export const ThemeModeProvider = ({ children }: { children: ReactNode }) => {
    const [mode, setMode] = useState<'light' | 'dark'>('light');
    const theme = useMemo(() => getTheme(mode), [mode]);
  
    const toggleTheme = () => {
      setMode((prevMode) => (prevMode === 'light' ? 'dark' : 'light'));
    };
  
    return (
      <ThemeModeContext.Provider value={{ toggleTheme, mode }}>
        <MUIThemeProvider theme={theme}>
          {/* CssBaseline kickstart an elegant, consistent, and simple baseline to build upon. */}
          <CssBaseline />
          {children}
        </MUIThemeProvider>
      </ThemeModeContext.Provider>
    );
  };