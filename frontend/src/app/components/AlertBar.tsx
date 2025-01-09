//React imports
import * as React from 'react';

//Mui imports
import Snackbar from '@mui/material/Snackbar';
import Alert from '@mui/material/Alert';

//Local components
import useStore from '../store/store'; 

interface AlertBarProps {   
    severity?: 'success' | 'error' | 'info' | 'warning';
    message: string;
}

export default function AlertBar({severity = 'success', message}: AlertBarProps) {
const { alertOpen, setAlertStatus } = useStore();

const handleClose = () => { 
    setAlertStatus(false);
};

  return (
      <Snackbar open={alertOpen} anchorOrigin={{vertical: 'top', horizontal: 'center'}} >
        <Alert
          severity={severity}
          variant="filled"
          sx={{ width: '100%' }}
          onClose={handleClose}
        >
            {message}
        </Alert>
      </Snackbar>
  );
}
