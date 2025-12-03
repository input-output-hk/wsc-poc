//React imports
import * as React from 'react';

//Mui imports
import Snackbar from '@mui/material/Snackbar';
import Alert from '@mui/material/Alert';
import Button from '@mui/material/Button';

//Local imports
import type { AlertInfo } from '../store/types';

type QueuedAlert = AlertInfo & { toastId: number };

type AlertBarProps = {
  alertInfo: AlertInfo;
  onClose: () => void;
};

export default function AlertBar({ alertInfo, onClose }: AlertBarProps) {
  const [queue, setQueue] = React.useState<QueuedAlert[]>([]);
  const lastSeenId = React.useRef<number | null>(null);

  React.useEffect(() => {
    if (alertInfo.open && alertInfo.id !== lastSeenId.current) {
      setQueue((prev) => [...prev, { ...alertInfo, toastId: alertInfo.id }]);
      lastSeenId.current = alertInfo.id;
      onClose();
    }
  }, [alertInfo, onClose]);

  const dismissToast = (toastId: number) => {
    setQueue((prev) => prev.filter((toast) => toast.toastId !== toastId));
  };

  if (queue.length === 0) {
    return null;
  }

  return (
    <>
      {queue.map((toast, index) => (
        <Snackbar
          key={toast.toastId}
          open
          anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
          autoHideDuration={6000}
          onClose={() => dismissToast(toast.toastId)}
          sx={{
            bottom: `${16 + index * 88}px !important`,
          }}
        >
          <Alert
            severity={toast.severity}
            variant="filled"
            sx={{ width: '100%', alignItems: 'center' }}
            onClose={() => dismissToast(toast.toastId)}
            action={
              toast.link ? (
                <Button
                  color="inherit"
                  size="small"
                  onClick={() => window.open(toast.link, '_blank', 'noopener,noreferrer')}
                >
                  {toast.actionText ?? 'View details'}
                </Button>
              ) : undefined
            }
          >
            {toast.message}
          </Alert>
        </Snackbar>
      ))}
    </>
  );
}
