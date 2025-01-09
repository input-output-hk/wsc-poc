'use client'
//React imports
import React, { useState } from 'react';

//MUI imports
import {Box} from '@mui/material';

//Local imports
import ContentTabs from './ContentTabs';
import WSTCommonButton from './WSTCommonButton';

interface TabContent {
  label: string
  content: React.ReactNode
  buttonLabel?: string
  onAction?: () => void
}

interface WalletCardProps {
  tabs: TabContent[]
}

export default function WalletCard({ tabs }: WalletCardProps) {
    const [tabValue, setTabValue] = useState(0);

    const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
        setTabValue(newValue);
    };

    const { content, buttonLabel, onAction } = tabs[tabValue]

  return (
    <div className="cardWrapper">
      <Box>
        <Box sx={{marginBottom: '36px'}}>
        <ContentTabs
            tabLabels={tabs.map((tab) => tab.label)}
            value={tabValue}
            onChange={handleTabChange}
          />
        </Box>
        {content}
      </Box>
      
      <Box sx={{ marginTop: 'auto', alignSelf: 'end' }}>
        {buttonLabel && (
          <WSTCommonButton
            text={buttonLabel}
            onClick={onAction}
            variant="outlined"
            size="small"
          />
        )}
      </Box>
    </div>
  );
}
