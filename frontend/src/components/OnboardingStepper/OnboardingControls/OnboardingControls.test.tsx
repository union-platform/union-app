// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { axe } from 'jest-axe';
import { RenderResult, render, fireEvent } from '@testing-library/react';
import React from 'react';
import OnboardingControls from './OnboardingControls';

const NEXT_BUTTON_TEST_ID = 'onboarding-controls-next';
const SKIP_BUTTON_TEST_ID = 'onboarding-controls-skip';

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

/* -------------------------------------------------------------------------------------------------
 *  OnboardingControls
 * -----------------------------------------------------------------------------------------------*/

describe('given a regular OnboardingControls', () => {
  let rendered: RenderResult;
  let nextButton: HTMLElement;
  let skipButton: HTMLElement;
  const mockSetScreenNumber = jest.fn();

  beforeEach(() => {
    rendered = render(<OnboardingControlsTest
      totalNumberOfScreens={3}
      screenNumber={3}
      setScreenNumber={mockSetScreenNumber}
    />);
    nextButton = rendered.getByTestId(NEXT_BUTTON_TEST_ID);
    skipButton = rendered.getByTestId(SKIP_BUTTON_TEST_ID);
  });

  it('should have no accessibility violations', async () => {
    expect(await axe(rendered.container)).toHaveNoViolations();
  });

  describe("when clicking 'next' button", () => {
    beforeEach(() => {
      fireEvent.click(nextButton);
    });

    it('should call setScreenNumber function', async () => {
      expect(mockSetScreenNumber).toBeCalled();
    });
  });

  describe("when clicking 'skip' button", () => {
    beforeEach(() => {
      fireEvent.click(skipButton);
    });

    it('should call setScreenNumber function', async () => {
      expect(mockSetScreenNumber).toBeCalled();
    });
  });
});

const OnboardingControlsTest = (props: React.ComponentProps<typeof OnboardingControls>) => (
  <OnboardingControls
    {...props}
  />
);
