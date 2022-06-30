// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import unboardingImage320 from './unboarding@320.png';
import unboardingImage600 from './unboarding@600.png';

export type OnboardingDataType = typeof OnboardingData[number];

const OnboardingData = [
  {
    title: 'Here is your favorite activity',
    description: 'Tell us about yourself and your goals - find new projects to your liking',
    imageURL320: unboardingImage320,
    imageURL600: unboardingImage600,
  },
  {
    title: 'Here is your team',
    description: 'Find a team or assemble your own - this is a place of attraction for creative people',
    imageURL320: unboardingImage320,
    imageURL600: unboardingImage600,
  },
  {
    title: 'Here is your implementation',
    description: 'Tell us about yourself and your goals - find new projects to your liking',
    imageURL320: unboardingImage320,
    imageURL600: unboardingImage600,
  },
  {
    title: 'Here you are not alone',
    description: 'Communicate with the team in the built-in messenger',
    imageURL320: unboardingImage320,
    imageURL600: unboardingImage600,
  },
];

export default OnboardingData;
