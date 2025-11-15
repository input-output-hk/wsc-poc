'use client'
//React imports
import React, { useEffect, useState } from 'react';

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
  buttonDisabled?: boolean
}

interface WalletCardProps {
  tabs: TabContent[]
}

export default function WalletCard({ tabs }: WalletCardProps) {
    const [tabValue, setTabValue] = useState(0);

    useEffect(() => {
      setTabValue((prev) => {
        if (tabs.length === 0) {
          return 0;
        }
        return Math.min(prev, tabs.length - 1);
      });
    }, [tabs]);

    const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
        setTabValue(newValue);
    };

    if (tabs.length === 0) {
      return null;
    }

    const safeTabIndex = Math.min(tabValue, tabs.length - 1);
    const { content, buttonLabel, onAction, buttonDisabled } = tabs[safeTabIndex]

  return (
    <div className="cardWrapper">
      <Box>
        <Box sx={{marginBottom: '36px'}}>
        <ContentTabs
            tabLabels={tabs.map((tab) => tab.label)}
            value={safeTabIndex}
            onChange={handleTabChange}
          />
        </Box>
        {content}
      </Box>
      
      <Box sx={{ marginTop: 'auto', alignSelf: 'end' }}>
        {buttonLabel && (
          <WSTCommonButton
            disabled={buttonDisabled}
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
