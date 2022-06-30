// SPDX-FileCopyrightText: 2021-2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from 'react';
import ReactDOM from 'react-dom';
import { BrowserRouter, Route, Routes } from 'react-router-dom';
import './index.css';
import App from './routes/App';
import Onboarding from './routes/Onboarding';
import Login from './routes/Login';

/**
 *  Starting point of the application
 */
ReactDOM.render(
  <React.StrictMode>
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<App />} />
        <Route path="onboarding" element={<Onboarding />} />
        <Route path="login" element={<Login />} />
      </Routes>
    </BrowserRouter>
  </React.StrictMode>,
  document.getElementById('root'),
);
