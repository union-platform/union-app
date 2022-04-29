// SPDX-FileCopyrightText: 2021-2022 Union
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Layout } from '@union-platform/ui';
import OnboardingData from '../data/onboardingData';
import OnboardingStepper from '../components/OnboardingStepper/OnboardingStepper';

/**
 *  Onboarding starting screens.
 */
const Onboarding = () => (
  <Layout>
    <OnboardingStepper redirectPathOnFinish="/" onboardingData={OnboardingData} />
  </Layout>
);

export default Onboarding;
