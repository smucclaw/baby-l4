% a raw implementation from Luger
% http://www.uoitc.edu.iq/images/documents/informatics-institute/Competitive_exam/Artificial_Intelligence.pdf
% section 2.4 on page 73--

:- use_module(library(clpfd)).

investment(savings)     :- savings_account(inadequate).
investment(stocks)      :- savings_account(  adequate), income(  adequate).
investment(combination) :- savings_account(  adequate), income(inadequate).

savings_account(  adequate) :- amount_saved(X),
                               dependents(Y),
                               minsavings(Y,MS),
                               X #>= MS.

savings_account(inadequate) :- amount_saved(X),
                               dependents(Y),
                               minsavings(Y,MS),
                               X #< MS.

minsavings(Y,MS) :- MS #= Y * 5000.

minincome(Y,MI) :- MI #= 15000 + 4000 * Y.

income(  adequate) :- earnings(X, steady),
                      dependents(Y),
                      minincome(Y,MI),
                      X #>= MI.

income(inadequate) :- earnings(X, steady),
                      dependents(Y),
                      minincome(Y,MI),
                      X #< MI.

income(inadequate) :- earnings(_, unsteady).

%% scenario 1
amount_saved(22000).
earnings(25000, steady).
dependents(3).
