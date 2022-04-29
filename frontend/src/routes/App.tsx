// SPDX-FileCopyrightText: 2021 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Link, Outlet } from 'react-router-dom';
import { LinearProgress } from '@union-platform/ui';
import { styled } from '../stitches.config';

import logo from './logo.svg';

const AppContainer = styled('div', {
  textAlign: 'center',
});

const Header = styled('div', {
  minHeight: '100vh',
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  justifyContent: 'center',
  fontSize: 'calc(10px + 2vmin)',
  color: '#282c34',
});

const App = () => (
  <AppContainer>
    <Header>
      <img src={logo} style={{ width: 200, height: 200 }} className="App-logo" alt="logo" />
      <LinearProgress value={30} progress={20} max={40} />
      <Link style={{ marginTop: 24 }} to="/login">Login</Link>
    </Header>
    <Outlet />
  </AppContainer>
);

export default App;
