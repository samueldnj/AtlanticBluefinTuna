// ><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><><>><><>><><>><>
// hierDelayAssess.cpp
// 
// A multi-stock Schnute-Deriso state-space spatially implicit Delay-Difference 
// model. Built to fit to the ABFT-MSE operating model data.
// 
// Authors: Samuel Johnson, Steven Rossi, and Sean Cox
// Initial Date: Dec 20, 2017
// Modified Date: May 24, 2018
// 
// 
// Features/Options:
//    - Conditioned on catch
//    - Spatially implicit (multi-area) with all stocks present in all areas
//        in time-varying proportions
// 
// Notes on variables:
//    - Arrays/vectors are named as X_ijk, where X is the array name and ijk are 
//      the indices of each dimension (row,column,slice,...)
//    - Most arrays are named as a single capital letter (B,N,I etc.) but in
//      some cases there may be a letter followed by an accent (Chat, wbar) or
//      if two words/abbreviations are concatenated, then camelCase is used
//      (e.g. initBioCode_s, lnM_s)
//    - Occasionally the camelCase convention is broken, when the variable in question 
//      is lower case by convention, e.g. lnq_os vs lnB0_s
//    - Greek letter tau is reserved for observation error/model variances,
//      while sigma is reserved for process error/model variances
// 
// Possible additional features:
//    - (auto-)correlated rDev_st series
//    - random walk in fishery dependent catchability
// 
// ><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><><>><><>><><>><>

#include <TMB.hpp>                                // Links in the TMB libraries
#include <iostream>

// addCompNoise() 
// Adds noise to compositional data assuming a
// multivariate logistic distribution
// inputs:    inputComp = vector of input compositional poportions
//            noise = vector of random errors to add to compositional data
// ouputs:    outputComp = new compositional data with error
// Usage:     Used when making random walks in compositional data
//            or for simulating noise (e.g. ageing error)
// Author:    S. D. N. Johnson
template<class Type>
vector<Type> addCompNoise(  vector<Type>  inputComp, 
                            vector<Type>  noise )
{
  int nComps = inputComp.size();
  // Take logs 
  vector<Type> logInputComp(nComps);
  logInputComp.fill(0.0);
  for( int c = 0; c < nComps; c++)
   logInputComp(c) = log( inputComp(c) );
  // Now add noise
  vector<Type> outputComp(nComps);
  outputComp = exp(logInputComp + noise);
  Type tmpTotal = outputComp.sum();
  outputComp /= tmpTotal;

  return outputComp;
  
} // End addCompNoise()

// posfun()
// Compares a quantity x to a threshold epsilon and if below the threshold, 
// increases x above the threshold and increments a penalty
// inputs:    x = value to be checked
//            eps = threshold
//            & pen = external penalty variable
// output:    y conditional on value of x-eps:
//            y = x if x > eps
//            y = eps / (2 - eps/x)
// side-effs: pen += 0.01 * (x - eps)^2
// Usage:     required for state space models when catch may
//            become larger than biomass under stochastic conditions.
// Source:    kaskr.github.com/adcomp wiki, modified by SDNJ
//            to actually make output > eps
template<class Type>
Type posfun(Type x, Type eps, Type &pen){
  pen += CppAD::CondExpLt(x, eps, Type(0.01) * pow(x-eps,2), Type(0.));
  return CppAD::CondExpGe(x, eps, x, eps/(Type(2.0)-eps/x));
}


// square()
// Shortcut wrapper for squaring quantities
// inputs:    x = quantity to be squared
// ouputs:    y = x^2
// Usage:     when squaring.
// Source:    Almost surely stolen from somewhere.
template <class Type> 
Type square(Type x){return pow(x,2);}
// VECTORIZE1_t(square)

// negLogLogisticNormal()
// Calculates the negative log density for a logistic normal
// distribution.
// inputs:    y = vector of observed proportions
//            p = vector of parameters (true class proportions)
//            var = variance of logistic normal distribution
// outputs:   nld = negative log density of logistic normal distribution
// Usage:     For computing the likelihood of observed compositional data
// Source:    S. D. N. Johnson
// Reference: Schnute and Haigh, 2007
template<class Type>
Type negLogLogisticNormal(  vector<Type> y, 
                            vector<Type> p, 
                            Type var )

{
  // Get dimension of MV distribution
  int N = y.size();

  // Take log var
  Type lnvar = log( var );

  // Variable to returning
  Type nld = 0;

  // Take geometric means
  Type ytilde = pow(y.prod(),1/N);
  Type ptilde = pow(p.prod(),1/N);

  // Add variance term
  nld += (N - 1) * lnvar / 2;

  // Now loop over dimensions, add residuals
  for( int i = 0; i < N; i++ )
    nld += square(log(y(i)/ytilde) - log(p(i)/ptilde))/2/var;

  return nld;
} // end negLogLogisticNormal()


// solveBaranovDD()
// Newton-Rhapson solver for Baranov catch equation for a population
// modeled with no age classes (e.g. Delay Difference formulation) at
// a given time step
// inputs:    nIter = number of NR iterations
//            Bstep = fraction of NR step (Jacobian) to take at each iteration
//            C = Catch
//            M = natural mortality
//            B = Biomass
//            & Z = total mortality (external variable)
//            & F = Fishing mortality (external variable)
// returns:   NA, void function
// Side-effs: variables passed as Z, F overwritten with total, fishing mortality
// Author:    Modified by S. D. N. Johnson from S. Rossi and S. P. Cox
template<class Type>
void solveBaranovDD(  int   nIter,
                      Type  Bstep,
                      Type  C,
                      Type  M,
                      Type  B,
                      Type& Z,
                      Type& F)
{
  Type f    = 0.;   // Function value
  Type J    = 0.;   // Jacobian
  Type newZ = 0.;   // Updated Z
  Type tmp  = 0.;   // predicted catch given F

  // Initial approximation of F
  F = C / (C+B);
  
  newZ = M + F;
  Z    = M + F;

  // Refine F
  for( int i=0; i<nIter; i++ )
  {
    // Total mortality
    Z     = newZ;
    newZ  = M;
    // Predicted catch given F
    tmp   = B*(1.-exp(-Z))*F/Z;

    // Function value: difference of pred - obs catch
    f   = C - tmp;
    // Jacobian
    J   = -B * ((1. - exp(-Z)) * M / pow(Z,2) + exp( -Z ) * F / Z);

   
    // Updated fishing mortality
    F -= Bstep * f / J;

    // Updated total mortality
    newZ += F;

  }  // end i

}  // end solveBaranovDD()




// Code all_of/any_of/none_of functions


// objective function
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Call namespaces //
  using namespace density;

  // -----------------  DATA SECTION ------------------- //
  // Data Structures
  DATA_VECTOR(lnObs_k);    // Observations
  DATA_VECTOR_INDICATOR(keep,lnObs_k);  // for generating OSA residuals
  DATA_IVECTOR(year_k);    // Year for each obs
  DATA_IVECTOR(gear_k);    // Fishery/survey for each obs
  DATA_IARRAY(iIdx_gt);   // Obs indices (k) for survey indices

  DATA_ARRAY(C_at);               // Catch data
  DATA_ARRAY(wbar_at);            // average weight observations by area/time
  DATA_IVECTOR(area_g);           // area codes for each fleet.
  
  // Growth model fixed pars
  DATA_IVECTOR(kage_s);           // age at recruitment
  DATA_INTEGER(nT_brood);         // number of brood years
  DATA_INTEGER(maxkage);          // max age at recruitment (fix int_max to avoid this)
  DATA_VECTOR(alpha_s);           // FW alpha
  DATA_VECTOR(rho_s);             // FW rho
  DATA_VECTOR(wk_s);              // weight at age k (is this wbar again? - no, this is for recruitment)

  // Priors
  DATA_ARRAY(prPropW_sp);

  // Model dimensions
  int nG = iIdx_gt.dim(0);           // No. of fleets g
  int nA = C_at.dim(0);           // No. of areas a
  int nS = rho_s.size();          // No. of stocks/species s
  int nT = C_at.dim(1);           // No. of time steps  t

  // Switches
  DATA_IVECTOR(initBioCode_s);    // initialise at unfished (0) or fished eqbm (1)
  DATA_IVECTOR(useWbar_a);        // Use average weight obs by area
  DATA_INTEGER(baranovIter);      // number of NR steps in solveBaranov
  DATA_SCALAR(baranovStep);       // size of NR steps in solveBaranov
  DATA_IVECTOR(idxType_g);        // code for what state variable to use for expected indices (see line 573)
  DATA_INTEGER(rType);

  // Biomass positive value penalty multiplier
  DATA_SCALAR(bioPenScale);       // Scalar multiple of bioPen added to obj fun

  // ------------------------ PARAMETER SECTION ------------------ //
  // Biological parameters //
  PARAMETER_VECTOR(logith_s);     // logit steepness
  PARAMETER_VECTOR(lnB0_s);       // log-scale unfished biomass
  PARAMETER_VECTOR(lnM_s);        // log-scale natural mortality rate

  // Stochastic parameters //
  // PARAMETER_VECTOR(rho_s);        // EIV proportion of variance for proc err
  
  // Observation model
  PARAMETER_VECTOR(lntau_g);      // fleet/gear observation error sd
  PARAMETER_VECTOR(tauW_a);       // Mean weight observation error sd

  // Random Effects //
  // Recruitment
  PARAMETER_ARRAY(rDev_st);       // Recruitment process errors
  PARAMETER_ARRAY(qDev_tg);       // Time varying catchability deviations
  PARAMETER_VECTOR(lnsigmaR_s);   // Recruitment proc error sd
  PARAMETER_VECTOR(lnsigmaQ_g);   // Catchability dev sd

  // Fishing Mortality
  PARAMETER_VECTOR(lnFinit_s);    // initialisation fishing mortality rate

  // biomass distribution parameters
  PARAMETER_VECTOR(logitPropW_s); // Percentage of stock s biomass in western area

  // Log catchability by gear
  PARAMETER_VECTOR(lnq_g);

  // Correlation in REs
  // PARAMETER_VECTOR(gammaR_s);     // AR correlation factor for R deviations

  // Prior distributions //
  // process error variance prior
  PARAMETER_VECTOR(sig2Prior);    // IG prior on recruitment dev variance
  PARAMETER_VECTOR(lnqbar_g);     // Fleet prior mean log catchability
  PARAMETER_VECTOR(lntauq_g);     // Survey catchability prior log variance
  PARAMETER_VECTOR(h_alpha);      // Steepness beta prior alpha parameter 
  PARAMETER_VECTOR(h_beta);       // Steepness beta prior beta parameter
  PARAMETER_VECTOR(tau2IGa_g);    // IG prior a par on obs error variance
  PARAMETER_VECTOR(tau2IGb_g);    // IG prior b par on obs error variance

  // // Multilevel M prior
  // PARAMETER(lnMbar);              // Average M
  // PARAMETER(lnsigma2M);           // M joint variance
  // PARAMETER(mM);                  // Average M prior mean
  // PARAMETER(sdM);                 // Average M prior sd
  // PARAMETER_VECTOR(sig2MPrior);   // parameters for sig2M prior
  // // Multilevel h prior
  // PARAMETER(logithbar);           // Joint steepness mean
  // PARAMETER(lnsigma2h);           // Joint steepness variance
  
  // PARAMETER_VECTOR(sig2hPrior);   // parameters for sig2h prior (use moment matching to compute sig2h)
  // // Multilevel q prior
  
  
  // PARAMETER(mq);                  // Average q prior mean
  // PARAMETER(sdq);                 // Average q prior sd
  // PARAMETER_VECTOR(tau2qPrior);   // parameters for tau2q prior (same for all surveys)
  // PARAMETER_VECTOR(tau2ObsPrior); // parameters for tau2_g prior (same for all surveys)

  // Transform pars //
  //vector<Type> h_s        = 0.2 + 0.78 * invlogit(logith_s);
  vector<Type> h_s        = invlogit(logith_s);
  vector<Type> B0_s       = exp(lnB0_s);
  vector<Type> M_s        = exp(lnM_s);
  vector<Type> tau_g      = exp(lntau_g);
  vector<Type> tau2_g     = exp(2.*lntau_g);
  vector<Type> sigmaR_s   = exp(lnsigmaR_s);
  vector<Type> sigma2R_s  = exp(2*lnsigmaR_s);
  vector<Type> sigmaQ_g   = exp(lnsigmaQ_g);
  vector<Type> qbar_g     = exp(lnqbar_g);
  vector<Type> tau2q_g    = exp(2*lntauq_g);
  vector<Type> Finit_s    = exp(lnFinit_s);
  vector<Type> propW_s    = invlogit(logitPropW_s);
  vector<Type> q_g        = exp(lnq_g);

  array<Type> I_gt(nG,nT); // Observations
  array<Type> predI_gt(nG,nT); // Predictions
  I_gt.fill(-1);
  predI_gt.fill(-1);

  // Define derived variables //
  // Model estimates of observed quantities
  array<Type>   C_st(nS,nT);          // Split area catch for stock and time
  
  // Eqbm values
  vector<Type>  S0_s(nS);             // unfished survival rate   
  vector<Type>  wbar0_s(nS);          // unfished average weight
  vector<Type>  N0_s(nS);             // unfished numbers
  vector<Type>  R0_s(nS);             // unfished recruitment
  
  // BH Recruitment pars
  vector<Type>  reca_s(nS);           // BH a parameter
  vector<Type>  recb_s(nS);           // BH b parameter
  
  // State variables
  array<Type>   B_st(nS,nT+1);          // stock biomass state variable
  array<Type>   N_st(nS,nT+1);          // stock numbers state variable
  array<Type>   R_st(nS,nT+1);          // Recruitment
  array<Type>   S_st(nS,nT);          // Survival
  array<Type>   wbarhat_st(nS,nT);    // estimated mean weight
  array<Type>   F_st(nS,nT);          // Fishing mortality
  array<Type>   Z_st(nS,nT);          // Total mortality
  array<Type>   D_st(nS,nT);          // Depletion
  array<Type>   U_st(nS,nT);          // Exploitation rate

  // Area based state variables
  array<Type>   B_at(nA,nT);          // Area biomass state variable
  array<Type>   N_at(nA,nT);          // Area numbers state variable
  array<Type>   B_sat(nS,nA,nT);      // Area biomass state variable
  array<Type>   N_sat(nS,nA,nT);      // Area numbers state variable
  array<Type>   wbarhat_at(nA,nT);    // estimated mean weight in each area
  array<Type>   wbarhat_sat(nS,nA,nT);// estimated mean weight in each area

  B_at.fill(0);
  N_at.fill(0);

  // Biomass distribution array
  array<Type>   bDist_sa(nS,nA);      // Proportion of stock s biomass in area a
  bDist_sa.setZero();

  // propCatch by stock/area/year
  array<Type>   propCatch_sat(nS,nA,nT);
  propCatch_sat.setZero();

  // West == 1
  // East == 0
  bDist_sa.col(1) = propW_s;
  bDist_sa.col(0) = 1 - propW_s;

  array<Type> rErr_st(nS,nT);
  rErr_st.fill(0);

  // Nuisance parameters
  vector<Type>   lnqhat_g(nG);        // log conditional MLE of Catchability (gear)
  vector<Type>   qhat_g(nG);          // conditional MLE of Catchability (gear)
  
  // Scalars
  Type          objFun    = 0.0;      // Objective function value
  Type          nlpProc   = 0.0;      // Process error likelihood
  Type          nllObs    = 0.0;      // Observation error likelihood
  Type          nlph      = 0.0;      // h joint neg log prior density
  Type          nlpW      = 0.0;      // Prior on prop west

  array<Type> nllI_gt(nG,nT);
  nllI_gt.fill(0);
  
  // Type          nlpM      = 0.0;      // M joint neg log prior density
  // Type          nlpq      = 0.0;      // q joint neg log prior density
  // Type          nlptau2_g = 0.0;      // tau2_g joint neg log prior density

  // calculate brood year adjustment 
  vector<int>   broodAdj  = maxkage - kage_s;

  // Observation model quantities
  array<Type>   ss_g(nG);             // Sum of squares
  array<Type>   validObs_g(nG);        // # of valid observations
  array<Type>   zSum_g(nG);           // sum of residuals

  // Negative biomass penalty
  Type          bioPen = 0.0;


  // Create a new rDev_st array that arranges
  // the deviations according to brood year
  array<Type> rDevBrood_st(nS,nT_brood);
  rDevBrood_st.fill(0.0);

  /* Estimation Procedure */
  // First, calculate recruitment and equilibrium parameters
  S0_s     = exp( -M_s );
  wbar0_s  = (alpha_s * S0_s + wk_s * (Type(1) - S0_s)) / (Type(1) - rho_s * S0_s);
  N0_s     = B0_s / wbar0_s;
  R0_s     = N0_s * (Type(1) - S0_s);
  reca_s   = Type(4) * h_s * R0_s / ( B0_s * ( Type(1) - h_s ) );
  recb_s   = ( Type(5) * h_s - Type(1) ) / ( B0_s * ( Type(1) -h_s ) );
  // use this loop to adjust deviations for brood year
  // SDNJ: revise so we can use a segment operation
  for( int s = 0; s < nS; s++ )
  {
    rDevBrood_st(s,broodAdj(s)) = rDev_st(s,0);
    for( int t=1; t<nT-1; t++ )
    {
      if( rType==0 )
        rDevBrood_st(s,t+broodAdj(s)) = rDev_st(s,t);
      else if( rType==1 )
        rDevBrood_st(s,t+broodAdj(s)) = rDevBrood_st(s,t+broodAdj(s)-1) + rDev_st(s,t);
    }
  }



  // ---------------- Population Dynamics (process model) ---------------- //
  for( int t = 0; t < (nT+1); t++ )
  {
    // Initialise population
    if( t == 0)
    {
      // Compute total mortality and initial survivorship
      // Borrow arrays here
      F_st.col(t) = exp(lnFinit_s);
      Z_st.col(t) = M_s + F_st.col(t);
      S_st.col(t) = exp( -1. * Z_st.col(t) );

      // Solve for fished equilibrium assuming 
      // initial F value is long-term eq value
      // Average weight
      wbarhat_st.col(t) = ( S_st.col(t) * alpha_s + wk_s*(1. - S_st.col(t) ) );
      wbarhat_st.col(t) /= 1. - rho_s*S_st.col(t);
      // Biomass
      B_st.col(t) = (S_st.col(t)*(alpha_s + rho_s*wbarhat_st.col(t)) + wbarhat_st.col(t)*(reca_s*wk_s - 1));
      B_st.col(t) /= recb_s * ( wbarhat_st.col(t) - rho_s*S_st.col(t)*wbarhat_st.col(t) - alpha_s * S_st.col(t));
      // Numbers
      N_st.col(t) = B_st.col(t) / wbarhat_st.col(t);
      // Recruitment
      R_st.col(t) = reca_s * B_st.col(t) / (1. + recb_s * B_st.col(t));
    }

    if( t > 0 )
    {
      for( int s = 0; s < nS; s++ )
      {
        // Recruitment
        // Compute expected recruitment
        if( t < kage_s(s) ) R_st(s,t) = R_st(s,0);
        else {
          int kage = kage_s(s);
          R_st(s,t) = reca_s(s) * B_st(s,t-kage) / (1. + recb_s(s) * B_st(s,t-kage));
        }
        // Now apply process errors
        //rErr_st(s,t) = exp( rDevBrood_st(s,t + broodAdj(s)) - sigma2R_s(s)/2 );
        // Random walk
        if( t<nT )
        {
          rErr_st(s,t) = exp( rDevBrood_st(s,t+broodAdj(s)) );
          R_st(s,t) *= rErr_st(s,t);
        }
      }

      // Update biomass and numbers
      B_st.col(t) = S_st.col(t-1) * (alpha_s * N_st.col(t-1) + rho_s * B_st.col(t-1) ) + wk_s * R_st.col(t);
      N_st.col(t) = S_st.col(t-1) * N_st.col(t-1) + R_st.col(t);

    }


    if( t < nT )
    {
      // Compute average weight
      wbarhat_st.col(t) = B_st.col(t) / N_st.col(t);
  
      // Split catch
      for( int a = 0; a < nA; a++ )
      {
        // Compute the total biomass of each stock in each area
        B_sat.col(t).col(a) = bDist_sa.col(a) * B_st.col(t);
        N_sat.col(t).col(a) = B_sat.col(t).col(a) / wbarhat_st.col(t);
  
        // Now use the total biomass to calculate the proportion
        // of area catch to assign to each stock.
        propCatch_sat.col(t).col(a) = bDist_sa.col(a) * B_st.col(t);
        propCatch_sat.col(t).col(a) /= propCatch_sat.col(t).col(a).sum();
      }
      // Compute stock specific catch for calculating Fs
      C_st.col(t) = propCatch_sat.col(t).matrix() * C_at.col(t).matrix();
  
      // Now compute area specific biomass and numbers, for
      // generating expected indices later, and returning
      // for the HCR
      for( int s = 0; s < nS; s++ )
      {
        B_at.col(t) += B_sat.col(t).transpose().col(s);
        N_at.col(t) += N_sat.col(t).transpose().col(s);
      }
  
      wbarhat_at.col(t)   = B_at.col(t)/N_at.col(t);
      wbarhat_sat.col(t)  = B_sat.col(t)/N_sat.col(t);
  
      // Calculate F using Newton-Rhapson solver
      for( int s = 0; s < nS; s++ )
      {
        // Check if biomass is greater than catch,
        // if not, penalise (requirement for conditioning on catch)
        B_st(s,t) = posfun( B_st(s,t), 1.05*C_st(s,t), bioPen );
        // Recast temp variables for solveBaranov()
        Type tmpF = 0.;
        Type tmpZ = 0.;
        Type tmpC = C_st(s,t);
        Type tmpB = B_st(s,t);
        Type tmpM = M_s(s);
        // Run solver
        solveBaranovDD( baranovIter, baranovStep, tmpC, tmpM, tmpB, tmpZ, tmpF);
        // Update F, Z, and S
        F_st(s,t) = tmpF;
        Z_st(s,t) = tmpZ;
        S_st(s,t) = exp( - Z_st(s,t) );
      }
  
      // Calculate depletion and exploitation rate
      D_st.col(t) = B_st.col(t) / B0_s;
      U_st.col(t) = C_st.col(t) / B_st.col(t); 
  
      // Calculate Catch from F
      // Chat_st(s,0) = B_st(s,0) * (1. - S_st(s,0)) * F_st(s,0) / Z_st(s,0);
    
    }
      
  }

  // ---------------- Observation Model ---------------- //
  // Calculate observation model likelihood, including catch
  // Fill with 0s
  validObs_g.fill(0.0);
  zSum_g.fill(0.0);
  ss_g.fill(0.0);
  // Loop over gear
  for( int g=0; g <nG; g++ )
  { 
    // years/timesteps
    for( int t=0; t<nT; t++ )
    {
      // only add a contribution if the data exists (It < 0 is missing)
      // We need to add a variable to change the state variable
      // being indexed here.
      // There could be FOUR in a 2 area model:
      // 1. Bio West (GOM)
      // 2. Bio East (MED)
      // 3. Bio Mixed (Area specific)
      // 4. Numbers mixed (Area specific)

      // In a 7 area model, the area-specific bio/numbers mixed
      // according to the bioDist array will work, since we will assume
      // that spawning areas are exclusive and the bioDist will take
      // care of that when generating the expected indices.

      int k = iIdx_gt(g,t);

      // West biomass
      if( idxType_g(g) == 2 )
        predI_gt(g,t) = B_st(1,t)*q_g(g);

      // East biomass
      if( idxType_g(g) == 1 )
        predI_gt(g,t) = B_st(0,t)*q_g(g);

      // Mixed biomass
      if( idxType_g(g) == 3 )
        predI_gt(g,t) = B_at(area_g(g),t)*q_g(g);

      // Mixed numbers
      if( idxType_g(g) == 4 )
        predI_gt(g,t) = N_at(area_g(g),t)*q_g(g);

      if ( k >= 0 ) 
      {
        I_gt(g,t) = exp(lnObs_k(k));

        nllI_gt(g,t) -= keep(k)*dnorm( lnObs_k(k),
                                       log(predI_gt(g,t)),
                                       tau_g(g),
                                       TRUE );

      } // end if k       
    } // end for t       
  }// end for g



  // Now add catch, mean weight, and age observations for CR Z estimation
  if( useWbar_a.sum() > 0 )
    for( int a = 0; a < nA; a++ )
      for( int t = 0; t < nT; t++ )
      {
        // As before, only add mean weight contributions if that data is provided
        if( wbar_at(a,t) > 0 & useWbar_a(a) == 1 )
        {
          nllObs -= dnorm( wbarhat_at(a,t), wbar_at(a,t), tauW_a(a), true);
        }
      }

  // Priors //
  // Calculate recruitment (process error) prior - correlated later??
  for( int s = 0; s < nS; s++ )
  {
    // Process errors
    vector<Type> devVec = rDev_st.matrix().row(s);
    nlpProc -= dnorm( devVec, Type(0), sigmaR_s(s), true).sum();
    
    // If initialising at fished eqbm, use a Jeffreys Prior
    // on initial F
//    if( initBioCode_s(s) == 1 )
//      nlpProc += lnFinit_s(s);

    // IG prior on sig2R
//    nlpProc += (sig2Prior(0)+Type(1))*2.*lnsigmaR_s(s) + sig2Prior(1)/sigma2R_s(s);

  }


  // Add steepness prior
  nlph -= ((h_alpha - 1) * log(h_s) + (h_beta - 1)*log(1 - h_s)).sum();
  

  for( int s = 0; s < nS; s++ )
    nlpW -= dnorm( propW_s(s), prPropW_sp(s,0), prPropW_sp(s,1), TRUE );

  // Obs error SD prior
  Type nlpObsErr = 0;
  nlpObsErr += ((tau2IGa_g+Type(1))*2*lntau_g+tau2IGb_g/tau2_g).sum();
  
  // Apply Jeffreys prior to B0
  Type nlpB0 = 0.;
  for( int s = 0; s < nS; s++ )
    nlpB0 -= log(1./B0_s(s));
  

  objFun += nllObs;
  objFun += nllI_gt.sum();
  objFun += nlph;
  objFun += nlpW;
  objFun += nlpProc;
  objFun += bioPenScale*bioPen;
  //objFun += nlpObsErr;
  //objFun +=  nlpB0;

  // transform lnqhat
  qhat_g = exp(lnqhat_g);

  /* Reporting Section */
  // First, report all data, derived and estimated variables
  // Data
  REPORT( I_gt );          // Survey indices
  REPORT( C_at);            // Observed Catch
  REPORT( wbar_at );        // Observed mean weight
  
  REPORT( predI_gt );          // Survey indices

  // Model dimensions
  REPORT( nG );             // No. of observational surveys o
  REPORT( nA );             // No. of observational surveys o
  REPORT( nS );             // No. of stocks/species s
  REPORT( nT );             // No. of time steps  t
  
  // Growth model fixed pars
  REPORT( kage_s );         // age at recruitment
  REPORT( maxkage );        // max age at recruitment
  REPORT( broodAdj );       // max age at recruitment
  REPORT( alpha_s );        // FW alpha
  REPORT( rho_s );          // FW rho
  REPORT( wk_s );           // weight at age of recruitment
  
  // Switches
  REPORT( initBioCode_s );  // initialise at unfished (0) or fished eqbm (1)
  REPORT( idxType_g );      // Spawning stock, area biomass or area numbers
  REPORT( area_g );

  // Biological parameters
  REPORT( h_s );            // steepness
  REPORT( B0_s );           // unfished biomass
  REPORT( M_s );            // natural mortality rate

  // Eqbm values
  REPORT( S0_s );           // unfished survival rate   
  REPORT( wbar0_s );        // unfished average weight
  REPORT( N0_s );           // unfished numbers
  REPORT( R0_s );           // unfished recruitment
  
  // BH Recruitment pars
  REPORT( reca_s );         // BH a parameter
  REPORT( recb_s );         // BH b parameter
  
  REPORT( lnq_g );
  REPORT( q_g );

  // State variables
  REPORT( B_st );           // stock biomass state variable
  REPORT( N_st );           // stock numbers state variable
  REPORT( C_st );           // stock catch (estimated)
  REPORT( R_st );           // Recruitment
  REPORT( S_st );           // Survival
  REPORT( wbarhat_st );     // estimated mean weight
  REPORT( F_st );           // Fishing mortality
  REPORT( Z_st );           // Total mortality
  REPORT( D_st );           // Depletion
  REPORT( U_st );           // Exploitation rate
  REPORT( Finit_s );        // Estimate of initial total mortality
  REPORT( propCatch_sat );

  REPORT( lnObs_k );
  REPORT( year_k );
  REPORT( gear_k );
  REPORT( iIdx_gt );

  // Area-specific biomass and numbers
  REPORT( B_at );           // area biomass state variable
  REPORT( N_at );           // area numbers state variable
  REPORT( B_sat );          // area biomass state variable
  REPORT( N_sat );          // area numbers state variable
  REPORT( wbarhat_at );     // estimated mean weight

  // Stochastic parameters
  REPORT( tauW_a );         // Mean weight observation error sd

  // Observation model
  REPORT( qhat_g );         // Survey catchability for each gear
  REPORT( tau2_g );         // Survey observation error variance

  // Random Effects
  REPORT( rDev_st );       // Recruitment process errors
  REPORT( rDevBrood_st );    // Recruitment process errors adj for BY
  REPORT( sigmaR_s );       // Recruitment proc error sd
  REPORT( sigmaQ_g );       // Recruitment proc error sd
  REPORT( F_st );           // Fishing mortality rates
  REPORT( rErr_st );

  // Catchability prior mean and variance
  REPORT( qbar_g );         // Survey mean log catchability
  REPORT( tau2q_g );        // Survey catchability log variance

  // Objective Function quantities
  REPORT( objFun );         // Objective function value
  REPORT( nllObs );         // Observation model neg log likelihood
  REPORT( nlpProc );        // Recruitment negative log prior density
  REPORT( bioPen );         // posfun biomass penalty
  REPORT( nlph );           // steepness negative log prior density (beta prior)
  REPORT( nllI_gt );



  REPORT(bDist_sa);
  REPORT(propW_s);


  // AD reporting for std errors //
  // Biological parameters
  ADREPORT( h_s );          // logit steepness
  ADREPORT( B0_s );         // log-scale unfished biomass
  ADREPORT( M_s );          // log-scale natural mortality rate

  // // BH Recruitment pars
  // ADREPORT( reca_s );       // BH a parameter
  // ADREPORT( recb_s );       // BH b parameter
  
  // State variables
  ADREPORT( B_at );         // biomass by area
  ADREPORT( B_st );         // biomass state variable
  ADREPORT( N_st );         // Numbers state variable
  ADREPORT( R_st );         // Recruitment
  ADREPORT( S_st );         // Survival
  ADREPORT( wbarhat_st );   // estimated mean weight
  ADREPORT( F_st );         // Fishing mortality
  ADREPORT( Z_st );         // Total mortality

  // Observation model
  ADREPORT( qhat_g );       // Survey and species catchability

  // Fishing mortality
  ADREPORT( F_st );         // Fishing mortality rates


  return objFun;
}
