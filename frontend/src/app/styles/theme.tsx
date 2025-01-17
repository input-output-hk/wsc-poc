'use client';
//Mui imports
import { createTheme } from "@mui/material/styles";

//Local file
import { lightModePalette } from "./palette";

export const getTheme = (mode: 'light') => createTheme({
    palette: {
      mode, 
      ...lightModePalette,
    },
    typography: {
      fontFamily: ['Roboto', 'sans-serif',].join(','),
      fontSize: 16,
      h1: {
        fontSize: "2.074rem",
      },
      h2: {
        fontSize: "1.728rem",
      },
      h3: {
        fontSize: "1.2rem",
      },
      h4: {
        fontSize: "16px",
      },
      h5: {
        fontSize: "16px",
      },
      h6: {
        fontSize: "1.2rem",
      },
      body1: {
        fontSize: '16px',
      },
    },
    components: {
      MuiTypography: {
        styleOverrides: {
          h1: ({theme}) => ({
            color: theme.palette.primary.onContainer,
            fontWeight: 600,
          }),
          h2: ({theme}) => ({
            color: theme.palette.onVariant.main,
            fontWeight: 500,
          }),
          h3: ({theme}) => ({
            color: theme.palette.primary.onContainer,
            fontWeight: 400,
          }),
          h4: ({theme}) => ({
            color: theme.palette.primary.onContainer,
            fontWeight: 500,
            marginBottom: '8px',
          }),
          h5: ({theme}) => ({
            color: theme.palette.primary.onContainer,
          }),
          body1: ({theme}) => ({
            color: theme.palette.onVariant.main,
          }),
        },
      },
      MuiButton: {
        styleOverrides: {
          root: {
            lineHeight: 1.25,
            minWidth: '90px',
            borderRadius: 100,
            textTransform: "none",
            boxShadow: "none",
            padding: "6px 10px",
          },
          contained: ({ theme }) => ({
            backgroundColor: theme.palette.primary.main,
            '&:hover': {
              background: `linear-gradient(
                0deg, 
                ${theme.palette.primary.conOpacity1}, 
                ${theme.palette.primary.conOpacity1}
                ), ${theme.palette.primary.main}`,
                boxShadow: `
                0px 1px 2px rgb(0, 0, 0, .3),
                0px 1px 3px 1px rgb(0, 0, 0, .15)}
              `,
              '&:active': {
                background: `linear-gradient( 
                  0deg,
                  ${theme.palette.primary.conOpacity2}, 
                  ${theme.palette.primary.conOpacity2}
                  ), ${theme.palette.primary.main}`,
                  boxShadow: "none !important", 
              },
            },
            '&:focus': {
              background: `linear-gradient( 
                0deg,
                ${theme.palette.primary.conOpacity2}, 
                ${theme.palette.primary.conOpacity2}
                ), ${theme.palette.primary.main}`,
                boxShadow: "none !important", 
            },
          }),
          outlined: ({ theme }) => ({
            border: `1px solid ${theme.palette.primary.main}`,
              color: `${theme.palette.primary.main}`,
              '&:hover': {
                background: `linear-gradient(
              0deg, 
              ${theme.palette.primary.mainOpacity1}, 
              ${theme.palette.primary.mainOpacity1}
              )`,
                '&:active': {
                  background: `linear-gradient(
                    0deg, 
                    ${theme.palette.primary.mainOpacity2}, 
                    ${theme.palette.primary.mainOpacity2}
                  )`,
                },
              },
              '&:focus': {
                background: `linear-gradient(
                  0deg, 
                  ${theme.palette.primary.mainOpacity2}, 
                  ${theme.palette.primary.mainOpacity2}
                )`,
              }
          }),
          text: ({ theme }) => ({
            color: `${theme.palette.onVariant.main}`,
              '&:hover': {
                color: `${theme.palette.secondary.main}`,
                background: `linear-gradient(
              0deg, 
              ${theme.palette.secondary.mainOpacity1}, 
              ${theme.palette.secondary.mainOpacity1}
              )`,
                '&:active': {
                  color: `${theme.palette.secondary.main}`,
                  background: `linear-gradient(
                    0deg, 
                    ${theme.palette.secondary.mainOpacity2}, 
                    ${theme.palette.secondary.mainOpacity2}
                  )`,
                },
              },  
              '&:focus': {
                background: `linear-gradient(
                  0deg, 
                  ${theme.palette.secondary.mainOpacity2}, 
                  ${theme.palette.secondary.mainOpacity2}
                )`,
              }
          }),
        },
      },
      MuiIconButton: {
        styleOverrides: {
          root: ({ theme }) => ({
            color: `${theme.palette.onVariant.main}`,
          }),
          colorSuccess: ({ theme }) => ({
            color: `${theme.palette.onVariant.main}`,
            '&:hover': {
              backgroundColor: 'transparent',
            },
            '&:focus': {
              background: 'transparent !important',
            },
            '&:active': {
              backgroundColor: 'transparent',
            },
          }),
          colorPrimary: ({ theme }) => ({
            '&:hover': {
              backgroundColor: `${theme.palette.primary.mainOpacity1}`,
            },
            '&:focus': {
              backgroundColor: `${theme.palette.primary.mainOpacity2}`,
            },
            '&:active': {
              backgroundColor: `${theme.palette.primary.mainOpacity2}`,
            },
          }),
          colorSecondary: ({ theme }) => ({
            color: `${theme.palette.onVariant.main}`,
            '&:hover': {
              backgroundColor: `${theme.palette.onSurface.mainOpacity1}`,
            },
            '&:focus': {
              backgroundColor: `${theme.palette.onSurface.mainOpacity2}`,
            },
            '&:active': {
              backgroundColor: `${theme.palette.onSurface.mainOpacity2}`,
            },
          }),
        },
      },
      MuiSvgIcon: {
        defaultProps: {
          fontSize: 'small', 
        },
        styleOverrides: {
          colorAction: ({ theme }) => ({
            color: `${theme.palette.onVariant.main}`,
          }),
        },
      },
      MuiInputBase: {
        styleOverrides: {
          root: ({ theme }) => ({
            color: `${theme.palette.onSurface.main}`,
            fontSize: '1rem',
            '&.Mui-error .MuiInputAdornment-root': {
              color: `${theme.palette.error.main}`,
            },
          }),
        },
      },
      MuiOutlinedInput: {
        styleOverrides: {
          root: ({ theme }) => ({
            ".MuiOutlinedInput-notchedOutline": {
              border: `1px solid ${theme.palette.outline.main}`,
            },
            ":hover .MuiOutlinedInput-notchedOutline": {
              borderColor: `${theme.palette.onSurface.main}`,
            },
            "&.Mui-focused .MuiOutlinedInput-notchedOutline": {
              borderColor: `${theme.palette.primary.main}`,
            },
            "&.Mui-error .MuiOutlinedInput-notchedOutline": {
              borderColor: `${theme.palette.error.main}`,
            },
          }),
        },
      },
      MuiInputLabel: {
        styleOverrides: {
          root: ({ theme }) => ({
            color: theme.palette.primary.main,
            fontSize: '1rem !important',
          }),
        },
      },
      MuiFormHelperText: {
        styleOverrides: {
          root: ({ theme }) => ({
            fontSize: '.75rem !important',
            marginLeft: '10px',
            color: `${theme.palette.onVariant.main}`,
          }),
        },
      },
      MuiTooltip: {
        styleOverrides: {
          tooltip: ({ theme }) => ({
            backgroundColor: `${theme.palette.onVariant.main}`,
          })
        },
      },
      MuiDivider: {
        styleOverrides: {
          root: ({ theme }) => ({
            borderColor: `${theme.palette.outlineVariant.main} !important`,
          })
        },
      },
      MuiAppBar: {
        styleOverrides: {
          root: ({ theme }) => ({
            backgroundColor: `${theme.palette.containerLowest.main}`,
            boxShadow: 'none',
          })
        },
      },
      MuiToolbar: {
        styleOverrides: {
          root: ({theme}) => ({
            borderBottom: `1px solid ${theme.palette.outlineVariant.main}`,
            paddingLeft: '0px !important',
            paddingRight: '16px !important',
            height: '48px !important',
            minHeight: '48px !important',
            justifyContent: 'space-between',
          }),
        },
      },
      MuiDrawer: {
        styleOverrides: {
          paper: ({theme}) => ({
            backgroundColor: theme.palette.containerLowest.main,
            borderRight: `1px solid ${theme.palette.outlineVariant.main}`,
          }),
        },
      },
      MuiTab: {
        styleOverrides: {
          root: ({ theme }) => ({
            textTransform: 'none',
            color: `${theme.palette.onVariant.main}`,
            ":hover": {
              background: `${theme.palette.primary.mainOpacity1}`,
            },
            ":focus": {
              background: 'none',
            },
          }),
        },
      },
      MuiListItemIcon: {
        styleOverrides: {
          root: ({theme}) => ({
            minWidth: '40px',
            color: theme.palette.onVariant.main,
          }),
        },
      },
      MuiListItemButton: {
        styleOverrides: {
          root: ({ theme }) => ({
            ":hover": {
              background: `${theme.palette.primary.mainOpacity1}`,
            },
            ":focus": {
              background: `${theme.palette.primary.mainOpacity2}`,
            },
          }),
        },
      },
      MuiTableContainer: {
        styleOverrides: {
          root: ({theme}) => ({
            backgroundColor: theme.palette.containerLowest.main,
            boxShadow: 'none',
            border: '1px solid #C9C6C6',
            height: '100%',
          }),
        },
      },
      MuiTableRow: {
        styleOverrides: {
          hover: () => ({
              backgroundColor: 'rgba(27, 27, 31, 0.1)',
          }),
        },
      }, 
      MuiTableCell: {
        styleOverrides: {
          stickyHeader: () => ({
              background: `linear-gradient(
                0deg, 
                rgba(57,82,205,0.12), 
                rgba(57,82,205,0.12)
                ), white`,
              fontSize: '14px',
          }),
          body: () => ({
              fontSize: '14px',
              color: '#46464F',
          }),
        },
      },
      MuiAlert: {
        styleOverrides: {   
          filledSuccess: () => ({
            backgroundColor: '#BAC3FF',
            color: '#00105B',
          }),
          filledError: () => ({
            backgroundColor: '#FFDBD2',
            color: '#3C0800',
          }),
          filledInfo: () => ({
            backgroundColor: '#FFF2AC',
            color: '#201C00',
          }),
        },
      },
    },
  });