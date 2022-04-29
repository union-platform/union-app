// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Button } from '@union-platform/ui';
import { useState } from 'react';
import { Link } from 'react-router-dom';
import { OnboardingDataType } from '../../data/onboardingData';
import { styled } from '../../stitches.config';
import OnboardingControls from './OnboardingControls/OnboardingControls';
import OnboardingScreen from './OnboardingScreen/OnboardingScreen';

const OnbordingControlsContainer = styled('div', {
  variants: {
    size: {
      mobile: {
        position: 'fixed',
        width: '100%',
        paddingBottom: '24px',
        bottom: 0,
        left: 0,
      },
      desktop: {
        position: 'initial',
        padding: 0,
        width: 'auto',
      },
    },
  },
});

const OnbordingContainer = styled('div', {
  minHeight: '100vh',
  display: 'flex',
  textAlign: 'center',
  flexDirection: 'column',
  alignItems: 'center',
  justifyContent: 'center',
  color: '#282c34',
});

interface OnboardingStepperProps {
  /**
   *  Array of onboarding screens
  */
  onboardingData: OnboardingDataType[],
  /**
   *  Array of onboarding screens
  */
  redirectPathOnFinish: string,
}

const OnboardingStepper = ({
  onboardingData, redirectPathOnFinish,
}: OnboardingStepperProps) => {
  const [screenNumber, setScreenNumber] = useState(0);
  const isNotLastScreen = screenNumber !== onboardingData.length - 1;

  return (
    <OnbordingContainer>
      {onboardingData.map((screen, ind) => (ind === screenNumber
        && (
        <OnboardingScreen
          key={`onboarding-screen-${screen.title}`}
          description={screen.description}
          title={screen.title}
          imageURL320={screen.imageURL320}
          imageURL600={screen.imageURL600}
        />
        )
      ))}

      <OnbordingControlsContainer size={{
        '@initial': 'mobile',
        '@bp1': 'desktop',
      }}
      >
        {isNotLastScreen
          ? (
            <OnboardingControls
              totalNumberOfScreens={onboardingData.length}
              screenNumber={screenNumber}
              setScreenNumber={setScreenNumber}
            />
          )
          : (
            <Link to={redirectPathOnFinish}>
              <Button data-testid="onboarding-stepper-final-button" variant="special">Start</Button>
            </Link>
          )}
      </OnbordingControlsContainer>

    </OnbordingContainer>
  );
};

export default OnboardingStepper;
