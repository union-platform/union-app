// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { axe } from 'jest-axe';
import { RenderResult, render } from '@testing-library/react';
import React from 'react';
import OnboardingScreen from './OnboardingScreen';

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

const OnboardingDataMock = {
  title: 'Here you are not alone',
  description: 'Communicate with the team in the built-in messenger',
  imageURL320: 'https://picsum.photos/320/320',
  imageURL600: 'https://picsum.photos/600/300',
};

/* -------------------------------------------------------------------------------------------------
 *  OnboardingScreen
 * -----------------------------------------------------------------------------------------------*/

describe('given a regular OnboardingScreen', () => {
  let rendered: RenderResult;

  beforeEach(() => {
    rendered = render(<OnboardingScreenTest {...OnboardingDataMock} />);
  });

  it('should have no accessibility violations', async () => {
    expect(await axe(rendered.container)).toHaveNoViolations();
  });
});

const OnboardingScreenTest = (props: React.ComponentProps<typeof OnboardingScreen>) => (
  <OnboardingScreen
    {...props}
  />
);
