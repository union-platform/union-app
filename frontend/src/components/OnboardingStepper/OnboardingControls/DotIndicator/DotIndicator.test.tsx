// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { axe } from 'jest-axe';
import { RenderResult, render } from '@testing-library/react';
import React from 'react';
import DotIndicator from './DotIndicator';

const TEST_NUMBER_OF_SCREENS = 3;
const CONTAINER_TEST_ID = 'dot-indicator-container';

/* -------------------------------------------------------------------------------------------------
 *  Dot Indicator
 * -----------------------------------------------------------------------------------------------*/

describe('given a regular DotIndicator with 3 dots', () => {
  let rendered: RenderResult;
  let dotContainer: HTMLElement;

  beforeEach(() => {
    rendered = render(<DotIndicatorTest
      currentScreen={2}
      numberOfScreens={TEST_NUMBER_OF_SCREENS}
    />);
    dotContainer = rendered.getByTestId(CONTAINER_TEST_ID);
  });

  it('should have no accessibility violations', async () => {
    expect(await axe(rendered.container)).toHaveNoViolations();
  });

  it('should have 3 dots', async () => {
    expect(dotContainer.children.length).toEqual(TEST_NUMBER_OF_SCREENS);
  });
});

const DotIndicatorTest = (props: React.ComponentProps<typeof DotIndicator>) => (
  <DotIndicator {...props} />
);
