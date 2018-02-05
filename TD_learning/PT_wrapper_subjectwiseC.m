% -------------------------
% This script estimates learning rates per subject using emotion ratings and nonlinear
% least squares optimization.
% ------------------------

clear all;
close all;
n_sub = 36;

%-------------------------------------------------------------------
% prepare output variable
learn_all = zeros(n_sub,1);
CI = zeros(n_sub,2);

% set path to working directory
pthdr = 'D:\\OneDrive\\Research\\GroupHelpAndPainRelief\\Github\\TD_learning\\';

% set starting value for learning parameter
learn_start=0.3;

%%% ********* %%%
%%% load data %%%
%%% ********* %%%

rwrd = csvread([pthdr '/rewards.csv']);
chc =  csvread([pthdr '/ratings.csv']);

% recode impression rating to reward inputs scale
chc_r = (chc-5)/5;
rating = chc_r;

%%% *************************** %%%
%%% estimation via nonlinear LS %%%
%%% *************************** %%%

%-------------------------------------------------------------------
% loop through subject

for sub = 1 :n_sub
    
    %-------------------------------------------------------------------
    % prepare data
    X = rwrd(:,sub);
    Yn = rating(:,sub);
    
    %-------------------------------------------------------------------
    % prepare options
    options = optimset('MaxFunEvals',100000,'MaxIter',100000,'Display','Iter','MaxPCGIter',100000);
    
    min_lim = [0];
    max_lim = [10];
    
    %-------------------------------------------------------------------
    % estimation
    [learn, resnorm, res, exitflag, output, lambdaOut, J]=lsqcurvefit(@RWfunc,learn_start,X,Yn,min_lim,max_lim,options);
    learn_all(sub) = learn;
    CI(sub,:)=nlparci(learn,res,J);
    
end

out_dat = [learn_all CI];
mean(out_dat)
csvwrite(['LearningRates' num2str(learn_start) '.csv'],out_dat);


%%% ****************************************************** %%%
%%% FOLLOW UP TESTS: are learning rates different from 0.3  %%%
%%% ****************************************************** %%%

expGroup = 1:18;
conGroup = 19:36;
IDX2 = [zeros(18,1); ones(18,1)]+1;

M_E = mean(out_dat(expGroup,:));
M_C = mean(out_dat(conGroup,:));

%-------------------------------------------------------------------
%%% Different from 0.3 for all
[h,p,ci,stats] = ttest(learn_all(:,1), 0.3)

%-------------------------------------------------------------------
%%% Different from 0.3 for G1
[h,p,ci,stats] = ttest(learn_all(expGroup,1), 0.3)

%-------------------------------------------------------------------
%%% Different from 0.3 for G2
[h,p,ci,stats] = ttest(learn_all(conGroup,1), 0.3)

