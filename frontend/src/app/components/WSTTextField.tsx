'use client';
//React Imports
import * as React from 'react';

//Mui imports
import TextField, { TextFieldProps } from '@mui/material/TextField';
import { InputAdornment } from '@mui/material';
import CancelOutlinedIcon from '@mui/icons-material/CancelOutlined';
import ErrorOutlineOutlinedIcon from '@mui/icons-material/ErrorOutlineOutlined';
import Tooltip from '@mui/material/Tooltip';

//local components
import IconButton from './WSTIconButton';

interface PHATextFieldProps {
    defaultValue: TextFieldProps['defaultValue'];
    error?: TextFieldProps['error'];
    fullWidth?: TextFieldProps['fullWidth'];
    helperText?: TextFieldProps['helperText'];
    label: TextFieldProps['label'];
}

export default function WSTTextField({defaultValue, error, fullWidth, helperText, label}: PHATextFieldProps) {
  const [isFocused, setIsFocused] = React.useState(false);
  const [value, setValue] = React.useState(defaultValue);
  const inputRef = React.useRef<HTMLInputElement>(null);

  const handleFocus = () => {
    setIsFocused(true);
  };

  const handleBlur = () => {
    setIsFocused(false);
  };

  const handleClear = () => {
    setValue('');
    setTimeout(() => {
      if (inputRef.current) {
        inputRef.current.focus();
      }
    }, 0);
  };

  return (
        <TextField
          sx={{marginBottom: '24px'}}
          inputRef={inputRef}
          value={value}
          onChange={(e) => setValue(e.target.value)}
          error={error}
          fullWidth={fullWidth}
          helperText={helperText}
          label={label}
          size="small"
          onFocus={handleFocus}
          onBlur={handleBlur}
          slotProps={{
            inputLabel: {color: "primary"},
            input: {endAdornment: (
              <InputAdornment position="end">
                {error && !isFocused ? (
                  <Tooltip title="Error Message"><ErrorOutlineOutlinedIcon /></Tooltip>
                ) : <IconButton icon={<CancelOutlinedIcon />} onMouseDown={handleClear} size="small" color='success' />}
              </InputAdornment>
            )}
          }}
        />
  );
}
