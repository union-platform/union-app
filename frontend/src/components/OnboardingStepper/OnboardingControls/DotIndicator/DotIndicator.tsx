// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { BrandColors, Dot, GrayShades } from '@union-platform/ui';

import { styled } from '../../../../stitches.config';

const DotIndicatorContainer = styled('div', {
  display: 'grid',
  gridAutoFlow: 'column',
  gap: 12,
  alignItems: 'center',
});

interface DotIndicatorProps {
  /**
   *  Current screen number
   */
  currentScreen: number;
  /**
   *  Number of screens that will be shown
   */
  numberOfScreens: number;
}

/**
 *  Dot indicator for onboarding screens
 */
const DotIndicator = ({ numberOfScreens, currentScreen }: DotIndicatorProps) => (
  <DotIndicatorContainer data-testid="dot-indicator-container">
    {Array.from(Array(numberOfScreens).keys()).map((val, ind) => (
      <Dot
        key={val.toString()}
        fill={ind <= currentScreen
          ? BrandColors.darkGreen
          : GrayShades.navigationGray}
      />
    ))}
  </DotIndicatorContainer>
);

export default DotIndicator;
