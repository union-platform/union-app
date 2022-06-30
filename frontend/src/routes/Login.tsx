// SPDX-FileCopyrightText: 2021-2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Header, Layout } from '@union-platform/ui';
import { Link } from 'react-router-dom';

import { styled } from '../stitches.config';
import logo from './logo.svg';

const AppContainer = styled('div', {
  textAlign: 'center',
});

const HeaderContainer = styled('div', {
  minHeight: '100vh',
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  justifyContent: 'center',
  fontSize: 'calc(10px + 2vmin)',
  color: '#282c34',
});

/**
 *  Login screen of the application
 */
const Login = () => (
  <Layout>
    <Header elevated={false} title="Login" />
    <AppContainer className="App">
      <HeaderContainer>
        <img src={logo} style={{ width: 300, height: 300 }} className="App-logo" alt="logo" />
        <p>
          Login
          {' '}
          <code>screen</code>
          {' '}
          will be here
        </p>
        <Link to="/onboarding">Log in</Link>
      </HeaderContainer>

    </AppContainer>
  </Layout>
);

export default Login;
