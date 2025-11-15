'use client';
//React Imports
import * as React from 'react';

//Mui imports
import TextField, { TextFieldProps } from '@mui/material/TextField';
import { InputAdornment } from '@mui/material';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';

//local components
import IconButton from './WSTIconButton';

interface PHATextFieldProps {
    value: TextFieldProps['defaultValue'];
    fullWidth?: TextFieldProps['fullWidth'];
    label: TextFieldProps['label'];
    sx?: TextFieldProps['sx'];
}

export default function CopyTextField({value, fullWidth, label, sx}: PHATextFieldProps) {
  const inputRef = React.useRef<HTMLInputElement>(null);

  const copyToClipboard = () => {
    if (inputRef.current) {
      inputRef.current.select();
      navigator.clipboard.writeText(inputRef.current.value);
    }
  }

  return (
        <TextField
          inputRef={inputRef}
          value={value}
          fullWidth={fullWidth}
          label={label}
          sx={sx}
          size="small"
          slotProps={{
            inputLabel: {color: "primary"},
            input: {endAdornment: (
              <InputAdornment position="end">
                  <IconButton onClick={copyToClipboard} icon={<ContentCopyIcon />} />
              </InputAdornment>
            )}
          }}
        />
  );
}
