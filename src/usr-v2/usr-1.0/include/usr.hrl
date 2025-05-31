%% Mobile user
-type msisdn() :: 1..9_999_999_9999.
-type userid() :: integer().
-type status() :: enabled | disabled. 
-type plan() :: prepay | postpay.
-type service() :: data | sms | lb.

-record(usr, {msisdn           :: msisdn(),
              id               :: userid(),
	          status = enabled :: status(),
	          plan = prepay    :: plan(), 
	          services = []    :: [service()]}).
