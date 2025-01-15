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
    placeholder?: TextFieldProps['placeholder'];
    value: TextFieldProps['value'];
    onChange: (event: React.ChangeEvent<HTMLInputElement>) => void; 
    error?: TextFieldProps['error'];
    fullWidth?: TextFieldProps['fullWidth'];
    helperText?: TextFieldProps['helperText'];
    label: TextFieldProps['label'];
    multiline?: TextFieldProps['multiline'];
    minRows?: number;
    maxRows?: number;
}

export default function WSTTextField({placeholder, value, onChange, error, fullWidth, multiline, minRows, maxRows, helperText, label}: PHATextFieldProps) {
  const [isFocused, setIsFocused] = React.useState(false);
  const inputRef = React.useRef<HTMLInputElement>(null);

  const handleFocus = () => {
    setIsFocused(true);
  };

  const handleBlur = () => {
    setIsFocused(false);
  };

  const handleClear = () => {
    onChange({ target: { value: '' } } as React.ChangeEvent<HTMLInputElement>);
    setTimeout(() => {
      if (inputRef.current) {
        inputRef.current.focus();
      }
    }, 0);
  };

  return (
        <TextField
          placeholder={placeholder}
          sx={{marginBottom: '24px'}}
          inputRef={inputRef}
          value={value}
          onChange={onChange}
          error={error}
          fullWidth={fullWidth}
          multiline={multiline}
          minRows={minRows}
          maxRows={maxRows}
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
