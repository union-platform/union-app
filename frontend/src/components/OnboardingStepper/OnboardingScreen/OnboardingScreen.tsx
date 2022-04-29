// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Heading3, Heading2, Text } from '@union-platform/ui';
import { OnboardingDataType } from '../../../data/onboardingData';
import { styled } from '../../../stitches.config';
import { useWindowSize } from '../../../utils/hooks';

const ContentContainer = styled('div', {
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
});

const TextContainer = styled('div', {
  marginTop: 24,
  marginBottom: 42,
  display: 'grid',
  gap: 16,
  variants: {
    size: {
      mobile: {
        width: '100%',
      },
      initial: {
        width: 400,
      },
    },
  },
});

const OnboardingScreen = ({
  title, description, imageURL320, imageURL600,
} : OnboardingDataType) => {
  const { width } = useWindowSize();

  const isNotMobile = !!(width && width > 600);

  return (
    isNotMobile
      ? (
        <ContentContainer key={`onboarding-screen-${title}`}>
          <img src={imageURL600} alt={`Onbording screen with title "${title}"`} />
          <TextContainer size="initial">
            <Heading2 weight="bold">{title}</Heading2>
            <Heading3 weight="regular">{description}</Heading3>
          </TextContainer>
        </ContentContainer>
      )
      : (
        <ContentContainer key={`onboarding-screen-${title}`}>
          <img src={imageURL320} alt={`Onbording screen with title "${title}"`} />
          <TextContainer size="mobile">
            <Heading3 weight="bold">{title}</Heading3>
            <Text weight="regular">{description}</Text>
          </TextContainer>
        </ContentContainer>

      ));
};

export default OnboardingScreen;
