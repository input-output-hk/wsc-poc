//React Imports
import * as React from 'react';

//MUI Imports
import Chip from '@mui/material/Chip';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';

//Local Imports
import useStore from '../store/store'; 
import { UserName } from '../store/types';

export default function ProfileSwitcher() {
  const [anchorEl, setAnchorEl] = React.useState<HTMLElement | null>(null);
  const currentUser = useStore(state => state.currentUser);
  const changeUserAccount = useStore(state => state.changeUserAccount);

  const handleClick = (event: React.MouseEvent<HTMLDivElement>) => {
    setAnchorEl(event.currentTarget as HTMLElement);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const handleSelect = (user: UserName) => {
    changeUserAccount(user);
    handleClose();
  };

  return (
    <>
      <Chip
        label={currentUser}
        onClick={handleClick}
        color="primary"
        deleteIcon={<KeyboardArrowDownIcon />}
        onDelete={handleClick}
      />
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleClose}
      >
        <MenuItem onClick={() => handleSelect('Mint Authority')}>Mint Authority</MenuItem>
        <MenuItem onClick={() => handleSelect('User A')}>User A</MenuItem>
        <MenuItem onClick={() => handleSelect('User B')}>User B</MenuItem>
      </Menu>
    </>
  );
}
