//Mui imports
import IconButton, { IconButtonProps } from '@mui/material/IconButton';

interface PHAButtonsProps {
    icon: React.ReactNode;  
    color?: 'default' | 'success' | 'primary' | 'secondary';
    disabled?: boolean;
    size?: IconButtonProps['size'];
    onClick?: React.MouseEventHandler<HTMLButtonElement>;
    onMouseDown?: React.MouseEventHandler<HTMLButtonElement>; 
  }

export default function WSTIconButton({ icon, color='primary', disabled, size='medium', onClick, onMouseDown }: PHAButtonsProps) {
  return (
      <IconButton color={color} disabled={disabled} size={size} onClick={onClick} onMouseDown={onMouseDown} disableFocusRipple disableRipple>
        {icon}
      </IconButton>
  );
}
