// SPDX-FileCopyrightText: 2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useEffect, useState } from 'react';

interface WindowSize {
  /**
   *  Width of the window
  */
  width?: number,
  /**
   *  Height of the window
  */
  height?: number,
}

/**
 *  Get current width and height of the window
 */
// eslint-disable-next-line import/prefer-default-export
export const useWindowSize = () => {
  const [windowSize, setWindowSize] = useState<WindowSize>({
    width: undefined,
    height: undefined,
  });
  useEffect(() => {
    const handleResize = () => {
      setWindowSize({
        width: window.innerWidth,
        height: window.innerHeight,
      });
    };

    new ResizeObserver(() => {
      handleResize();
    }).observe(document.body);

    handleResize();
  }, []);
  return windowSize;
};
