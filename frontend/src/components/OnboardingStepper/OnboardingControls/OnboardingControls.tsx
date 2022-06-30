// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Button, GrayShades } from '@union-platform/ui';
import { styled } from '../../../stitches.config';
import DotIndicator from './DotIndicator/DotIndicator';

interface OnboardingControlsProps {
  /**
   *  Total number of onboarding screens
  */
  totalNumberOfScreens: number;
  /**
   *  Current screen number
  */
  screenNumber: number;
  /**
   *  Screen number setter
  */
  setScreenNumber: (_a: number) => void;
}

const OnbordingControlsContainer = styled('div', {
  display: 'grid',
  gridAutoFlow: 'column',
  gap: 42,
  alignItems: 'center',
});

const SkipButton = styled(Button, {
  color: GrayShades.navigationGray,
});

const OnboardingControls = ({
  totalNumberOfScreens, screenNumber, setScreenNumber,
}: OnboardingControlsProps) => (
  <OnbordingControlsContainer>
    <SkipButton
      data-testid="onboarding-controls-skip"
      onClick={() => setScreenNumber(totalNumberOfScreens - 1)}
      variant="text"
    >
      Skip
    </SkipButton>
    <DotIndicator
      numberOfScreens={totalNumberOfScreens}
      currentScreen={screenNumber}
    />
    <Button
      data-testid="onboarding-controls-next"
      onClick={() => setScreenNumber(screenNumber + 1)}
      variant="text"
    >
      Next
    </Button>
  </OnbordingControlsContainer>
);

export default OnboardingControls;
