'use client';

//React imports
import * as React from 'react';

//Mui imports
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';

interface ContentTabsProps {
  value: number; 
  onChange: (event: React.SyntheticEvent, newValue: number) => void;
  tabLabels: string[];
}

export default function ContentTabs({ value, onChange, tabLabels }: ContentTabsProps) {

  return (
    <Tabs
      value={value}
      onChange={onChange}
    >
      {tabLabels.map((label, index) =>
      <Tab key={index} label={label}/>  
      )}
    </Tabs>
  );
};