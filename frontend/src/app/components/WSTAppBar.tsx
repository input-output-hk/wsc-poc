'use client'
//NextJS Imports
import Image from 'next/image';

//MUI Imports
import { Box, Typography } from '@mui/material';
import AppBar from '@mui/material/AppBar';
import Toolbar from '@mui/material/Toolbar';

//Local file
import ProfileSwitcher from './ProfileSwitcher';

export default function WSTAppBar() {
  return (
      <AppBar position="fixed" sx={{marginBottom: '20px', ml: `200px`, zIndex: 1201}}>
        <Toolbar disableGutters={true}>
            <Box sx={{display: 'flex', alignItems: 'center'}}>
                <Image
                src="/assets/WST_logo.png"
                width={53}
                height={39}
                alt="Logo for Wyoming Stable Token"
                />
                <Typography variant='h3'>Wyoming Stable Token</Typography>
            </Box>
            <ProfileSwitcher />
        </Toolbar>
      </AppBar>
  );
};
