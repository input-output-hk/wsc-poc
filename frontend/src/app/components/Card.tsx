'use client'
//React imports
import React, { useState } from 'react';

//MUI imports
import {Box} from '@mui/material';

//Local imports
import ContentTabs from './ContentTabs';
import WSTCommonButton from './WSTCommonButton';

interface WalletCardProps {
  tabLabels: string[]; 
  cardContentSection1: React.ReactNode;
  cardContentSection2: React.ReactNode;
  bttnLabels: string[]; 
  onAction1: () => void;
  onAction2?: () => void;
}

export default function WalletCard({ tabLabels, cardContentSection1, cardContentSection2, bttnLabels, onAction1, onAction2 }: WalletCardProps) {
    const [tabValue, setTabValue] = useState(0);

    const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
        setTabValue(newValue);
    };

  return (
    <div className="cardWrapper">
      <Box>
        <Box sx={{marginBottom: '36px'}}>
            <ContentTabs tabLabels={tabLabels} value={tabValue} onChange={handleTabChange} />
        </Box>
        {tabValue === 0 && (cardContentSection1)}
        {tabValue === 1 && (cardContentSection2)}
      </Box>
      
      <Box sx={{marginTop: 'auto', alignSelf: 'end'}}>
      {tabValue === 0 && <WSTCommonButton text={bttnLabels[0]} onClick={onAction1} variant='outlined' size='small'/>}
      {(tabValue === 1 && bttnLabels.length > 1) ? <WSTCommonButton text={bttnLabels[1]} onClick={onAction2} variant='outlined' size='small'/> : <></>}
      </Box>
    </div>
  );
}
