//Mui imports
import Button, { ButtonProps } from '@mui/material/Button';
import CircularProgress from '@mui/material/CircularProgress';

interface PHAButtonProps {
    disabled?: boolean;
    size?: ButtonProps['size'];
    fullWidth?: boolean;
    startIcon?: React.ReactNode;
    variant?: ButtonProps['variant']; 
    text?: string;
    onClick?: React.MouseEventHandler<HTMLButtonElement>;
    loading?: boolean;
  }

export default function WSTCommonButton({
  disabled,
  size = 'medium',
  fullWidth,
  startIcon,
  variant = 'contained',
  text,
  onClick,
  loading = false,
}: PHAButtonProps) {
  return (
    <Button
      disabled={disabled || loading}
      size={size}
      fullWidth={fullWidth}
      startIcon={loading ? <CircularProgress size={16} color="inherit" /> : startIcon}
      variant={variant}
      disableFocusRipple
      disableRipple
      onClick={onClick}
    >
      {text}
    </Button>
  );
}
