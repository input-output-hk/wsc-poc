//Mui imports
import Button, { ButtonProps } from '@mui/material/Button';

interface PHAButtonProps {
    disabled?: boolean;
    size?: ButtonProps['size'];
    fullWidth?: boolean;
    startIcon?: React.ReactNode;
    variant?: ButtonProps['variant']; 
    text?: string;
    onClick?: React.MouseEventHandler<HTMLButtonElement>; 
  }

  export default function WSTCommonButton({ disabled, size='medium', fullWidth, startIcon, variant='contained', text, onClick }: PHAButtonProps) {
    return (
      <Button disabled={disabled} size={size} fullWidth={fullWidth} startIcon={startIcon} variant={variant} disableFocusRipple disableRipple onClick={onClick}>{text}</Button>
    );
  }