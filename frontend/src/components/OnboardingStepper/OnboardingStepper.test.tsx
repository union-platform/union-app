// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { axe } from 'jest-axe';
import { RenderResult, render } from '@testing-library/react';
import OnboardingStepper from './OnboardingStepper';

global.ResizeObserver = class ResizeObserver {
  cb: any;

  constructor(cb: any) {
    this.cb = cb;
  }

  observe() {
    this.cb([{ borderBoxSize: { inlineSize: 0, blockSize: 0 } }]);
  }

  unobserve() {}

  disconnect() {}
};

const OnboardingDataMock = [
  {
    title: 'Here is your favorite activity',
    description: 'Tell us about yourself and your goals - find new projects to your liking',
    imageURL320: 'https://picsum.photos/320/320',
    imageURL600: 'https://picsum.photos/600/300',
  },
  {
    title: 'Here is your team',
    description: 'Find a team or assemble your own - this is a place of attraction for creative people',
    imageURL320: 'https://picsum.photos/320/320',
    imageURL600: 'https://picsum.photos/600/300',
  },
  {
    title: 'Here is your implementation',
    description: 'Tell us about yourself and your goals - find new projects to your liking',
    imageURL320: 'https://picsum.photos/320/320',
    imageURL600: 'https://picsum.photos/600/300',
  },
  {
    title: 'Here you are not alone',
    description: 'Communicate with the team in the built-in messenger',
    imageURL320: 'https://picsum.photos/320/320',
    imageURL600: 'https://picsum.photos/600/300',
  },
];

/* -------------------------------------------------------------------------------------------------
 *  OnboardingStepper
 * -----------------------------------------------------------------------------------------------*/

describe('given a regular OnboardingStepper', () => {
  let rendered: RenderResult;

  beforeEach(() => {
    rendered = render(<OnboardingStepperTest redirectPathOnFinish='/' onboardingData={OnboardingDataMock}/>);
  });

  it('should have no accessibility violations', async () => {
    expect(await axe(rendered.container)).toHaveNoViolations();
  });
});

const OnboardingStepperTest = (props: React.ComponentProps<typeof OnboardingStepper>) => (
  <OnboardingStepper {...props} />
);
