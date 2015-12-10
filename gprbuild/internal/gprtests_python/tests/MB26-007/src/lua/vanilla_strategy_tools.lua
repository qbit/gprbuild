function is_empty(t)
    if type(t)~="table" then
        error("is_empty must be called on tables")
    end
    return (next(t)==nil)
end

-----------------------------------------------------
------------- Portfolio object
-----------------------------------------------------

local portfolio = {}
local portfolio_count = 0

-------------------------------------------
-- debug function print for portfolio_object
-------------------------------------------
function portfolio:print()
    print(string.format("----portfolio_compo of %s",self.name))
    portfolio_items = self.portfolio_items
    if portfolio_items~=nil then
        for idx in pairs(portfolio_items) do
            item = portfolio_items[idx]
            option=item.option
            print("option : friction_id=",option.friction_id)
            print(string.format("weight : %f",item.weight))
        end
    end
    print("----end of portfolio_compo of %s",self.name)
end

---------------------------------------
-- get_portfolio_items
---------------------------------------
function portfolio:get_portfolio_items()
    return self.portfolio_items
end

-------------------------------------------------------------------------------------------------------
-- append : take a table as argument with
-- fields option, weight, underlying, portfolio
-- enables to add to portfolio either a single option
-- either a sub portfolio
-- when an option (or options of a sub portfolio) enters into the portfolio
-- their names change. This name enables afterwards in ada to distinguish
-- between different kind of options, when other parameters do not allow it
-- for instance : BNPIBWJH_IDX where two similar options are present in the portfolio at each time step
-- and they are only different because of their trading constraints
-- we prefix new option name by the portfolio_name, thus we can know the booking's depth
--------------------------------------------------------------------------------------------------------

function portfolio:append (args)--option, weight, underlying, portfolio
    under = args.underlying
    option = args.option
    weight = args.weight
    sub_portfolio = args.portfolio
    if sub_portfolio~=nil then
        assert (sub_portfolio.entity_kind == "vanilla_portfolio")
    else
        assert (self.entity_kind == "vanilla_portfolio")
        assert (under.entity_kind == "vs_underlying")
        assert (option.entity_kind == "vanilla_option")
    end
    if option~=nil and sub_portfolio~=nil then
        error("you can not append an option and a portfolio at the same time")
    end
    if self.n_items == 0 then
        self.portfolio_items = {}
    end

    if sub_portfolio~=nil then--case of sub portfolio
        portfolio_items = sub_portfolio:get_portfolio_items()
        for key in pairs(portfolio_items) do
            self.n_items = self.n_items+1


            option = portfolio_items[key]["option"]
            table.insert(self.portfolio_items,{option = option, weight = portfolio_items[key]["weight"], underlying = portfolio_items[key]["underlying"]})
        end
    else--case of a single option
        self.n_items = self.n_items+1



        table.insert (self.portfolio_items,
                 {option = option, weight = weight, underlying = under})
    end
end

------------------------------------------------------------------------------------------------------
-- new : create a new (empty) portfolio : internal function
-- portfolio is named, either through an internal counter portfolio_count, either by the provided name
-------------------------------------------------------------------------------------------------------
portfolio.new = function (name)
   n_items = 0
   name = name or nil

   self = {}
   self.entity_kind = "vanilla_portfolio"
   --if next(portfolio_items)~=nil then
   --only define portfolio_items when it's not empty, otherwise in current implementation of get_dictionary_from_table, we've got a problem
      -- self.portfolio_items = portfolio_items
    --end
   self.n_items = n_items
    if name==nil then
        self.name = string.format("portfolio_%d",portfolio_count)
    else
        self.name = name
    end
   portfolio_count = portfolio_count+1
   self.append = portfolio.append
   setmetatable(self,{__index=portfolio})

   return self
end

----------------------------------------------------------
--make_portfolio : the public one to be used in lua scripts
-- we do not declare it as local function because we also use it
--through protected_call in ada and it does not work if function is
-- local to vanilla_strategy_tools (maybe a well placed import in ada is missing)
----------------------------------------------------------
function make_portfolio()
    ptf = portfolio.new()
    return ptf
end

--------------
--vs_underlying
--------------
local vs_underlying = {}

--------------------------
-- print : debug function
--------------------------
function vs_underlying:print()
    print("vs_underlying with ",self.underlying_name,self.underlying_type)
end

-------------------------------------------------------
-- make_underlying : the one to be called in lua script
-------------------------------------------------------
function make_underlying (underlying_name, underlying_type)
   underlying_type = underlying_type or "pr"
   self = {
      entity_kind = "vs_underlying",
      underlying_name = underlying_name,
      underlying_type = underlying_type
   }
    setmetatable(self,{__index=vs_underlying})
    return self
end

------------------
--vanilla_option
------------------
local vanilla_option = {}

-----------------------
-- make_option : the one to be called in lua script
-----------------------
function make_option (args)--kind , strike, maturity, friction, name, hedge, last_trading_date, friction_id, hedge_id, option_id
   kind = args.kind
   strike = args.strike
   maturity = args.maturity
   friction = args.friction
args_friction_id = args.friction_id
args_hedge_id = args.hedge_id
   hedge = args.hedge
   name = args.name
   last_trading_date = args.last_trading_date
   if last_trading_date == nil then
    last_trading_date = 0.0--default value in vanilla_strategy
   end
   if hedge~=nil then
        hedge_id = hedge.hedge_id
elseif args_hedge_id then
hedge_id = args_hedge_id
    else
        hedge_id = 0
    end
    if friction~=nil then
        friction_id = friction.friction_id
elseif args_friction_id~=nil then
friction_id =args_friction_id
    else
        friction_id = 0
    end
    self = {
      entity_kind = "vanilla_option",
      kind = kind,
      strike = strike,
      maturity = maturity,
      friction_id = friction_id,
      hedge_id = hedge_id,
      last_trading_date = last_trading_date,
      name = name or string.format("%s_%f_%f",kind,strike,maturity)--we always assign a name to option
   }
if args.option_id~=nil then
self.option_id = args.option_id
else
   self.option_id = gprime.set_object(self,1) -- 1 for option object
end
   setmetatable(self,{__index=vanilla_option})
   return self
end

----------------------------------------------------------------------------
-- rename function : it is called each time an option enters a new portfolio
----------------------------------------------------------------------------
function vanilla_option:rename(new_name)
    self.name = new_name
end

-------------------------------
-- debug function print
-------------------------------
function vanilla_option:print()
    print(string.format("vanilla_option : type %s, strike %f, maturity %f, name %s", self.kind, self.strike, self.maturity, self.name))
end


--------------------------------
-- make_adaptative_friction
--------------------------------
local function make_adaptative_friction(name, structure_position, long_positions_friction, short_positions_friction)
      friction ={
         entity_kind = "adaptative_friction",
         name = name,
         structure_position = structure_position,
         long_positions_friction = long_positions_friction,
         short_positions_friction = short_positions_friction}
      friction.friction_id = gprime.set_object(friction,2) -- 2 for friction object
      return friction
end

------------------------------------
-- make_implicit_volatility_friction
------------------------------------
local function make_implicit_volatility_friction (name,
                                                  relative_vol_spread,
                                                  absolute_vol_spread,
                                                  vol_spread_cap,
                                                  vol_spread_floor)
   relative_vol_spread = relative_vol_spread or 0.0
   absolute_vol_spread = absolute_vol_spread or 0.0
   vol_spread_cap = vol_spread_cap or 1000.0
   vol_spread_floor = vol_spread_floor or 0.0

   friction = {
      entity_kind = "implicit_volatility_friction",
      name = name,
      relative_vol_spread = relative_vol_spread,
      absolute_vol_spread = absolute_vol_spread,
      vol_spread_cap = vol_spread_cap,
      vol_spread_floor = vol_spread_floor
   }
   friction.friction_id = gprime.set_object(friction,2) -- 2 for friction object

   return friction
end

-------------------
-- make_friction --
-------------------

local function make_friction (name, entity_kind, friction_args)

   friction = {entity_kind = entity_kind, name = name}
   for k, v in pairs(friction_args) do friction [k] = v end
   friction.friction_id = gprime.set_object(friction,2) -- 2 for friction object

   return friction
end

-------------------------
-- make_price_friction --
-------------------------

local function make_price_friction (name, friction_args)

   friction = {entity_kind = "price_friction", name = name}
   for k, v in pairs(friction_args) do friction [k] = v end
   friction.friction_id = gprime.set_object(friction,2) -- 2 for friction object

   return friction
end

------------------------------
-- make_delta_bs_spot_position
------------------------------


local function make_delta_bs_spot_position (underlying)
   hedge = {
      entity_kind = "delta_bs_spot_position_hedge",
      underlying = underlying,
   }
   hedge.hedge_id = gprime.set_object(hedge,3) -- 3 for hedge object
   return hedge
end

local function make_delta_skew_spot_position (name,
                                              underlying,
                                              skew_computation_strike_list,
                                              delta_skew_floor,
                                              delta_skew_cap)
   t = {
      entity_kind = "delta_skew_spot_position_hedge",
      name = name,
      skew_computation_strike_list = skew_computation_strike_list,
      underlying = underlying,
   }
   if delta_skew_floor ~= nil then
	t.delta_skew_floor = delta_skew_floor
   end
   if delta_skew_cap ~= nil then
	t.delta_skew_cap = delta_skew_cap
   end
   t.hedge_id = gprime.set_object(t,3) -- 3 for hedge object
   return t
end


local function make_delta_bs_converse_position (name, underlying)
   hedge = {
      entity_kind = "delta_bs_converse_position_hedge",
      name = name,
      underlying = underlying,
   }
   hedge.hedge_id = gprime.set_object(hedge,3) -- 3 for hedge object
   return hedge
end

local function make_delta_skew_converse_position (name,
                                                  underlying,
                                                  skew_computation_strike_list,
                                                  delta_skew_floor,
                                                  delta_skew_cap)
   t = {
      entity_kind = "delta_skew_converse_position_hedge",
      name = name,
      skew_computation_strike_list = skew_computation_strike_list,
      underlying = underlying,
   }
   if delta_skew_floor ~= nil then
	t.delta_skew_floor = delta_skew_floor
   end
   if delta_skew_cap ~= nil then
	t.delta_skew_cap = delta_skew_cap
   end
   t.hedge_id = gprime.set_object(t,3) -- 3 for hedge object
   return t
end

local function make_delta_converse_position (name, underlying)
   hedge = {
      entity_kind = "delta_converse_position_hedge",
      name = name,
      underlying = underlying,
   }
   hedge.hedge_id = gprime.set_object(hedge,3) -- 3 for hedge object
   return hedge
end

-----------------------------
--trading constraints objects
-----------------------------
local trading_constraints = {}

local function make_trading_constraints (args)
tc = {}
setmetatable(tc,{__index=trading_constraints})
tc.weight_value = args.weight_value
if args.spot_upper_limit~=nil then
    tc.spot_upper_limit = 0.01*args.spot_upper_limit
end
tc.vol_underlying = args.vol_underlying
tc.vol_option_underlying = args.vol_option_underlying
tc.vol_option = args.vol_option
tc.premium_upper_limit = args.premium_upper_limit
if args.spot_lower_limit~=nil then
    tc.spot_lower_limit = 0.01*args.spot_lower_limit
end
tc.moving_average_length = args.moving_average_length
tc.moving_average_last_date_lag = args.moving_average_last_date_lag
if args.vol_upper_limit~=nil then
    tc.vol_upper_limit = 0.01*args.vol_upper_limit
end
if args.vol_lower_limit~=nil then
    tc.vol_lower_limit = 0.01*args.vol_lower_limit
end
return tc
end



--------------
--get_trading_constraints_weight
--------------
function trading_constraints:get_trading_constraints_weight(args)
    --option is traded
    local function option_is_traded(premium,vol,moving_average_perf)
        if self.premium_condition~=nil then
            if not(self.premium_min < premium and premium<=self.premium_max) then
                return false
            end
        end

        if self.vol_underlying~=nil or self.vol_option~=nil then
            if not(self.vol_lower_limit < vol and vol <= self.vol_upper_limit) then
                return false
            end
        end

        if tc.spot_upper_limit~=nil then
            if not(self.spot_lower_limit < moving_average_perf and moving_average_perf <= self.spot_upper_limit) then
                return false
            end
        end

        return true
    end
    --end of option_is_traded

    under = args.under
    current_date = gprime.get_current_date()
    spot = gprime.get_spot(under.underlying_name,current_date)
    moving_average = gprime.get_asian_fixing(under, current_date, self.moving_average_length, "[1bd]", self.moving_average_last_date_lag)
    pricing_result = gprime.price(args.option,under)
    if self.vol_underlying~=nil then
        vol = 0.01*gprime.get_spot(self.vol_underlying,current_date)
    elseif self.vol_option~=nil then
        vol_option = self.vol_option
        vol_pricing_result = gprime.price(self.vol_option,self.vol_option_underlying)
        vol = gprime.get_greek(vol_pricing_result,"implicit_volatility_without_frictions")
    end

    if spot~=0 then
    greek_premium = gprime.get_greek(pricing_result,"premium")
    relative_premium = greek_premium/spot
    end

    if moving_average~=0 then
        moving_average_perf = spot/moving_average-1
    end

    if option_is_traded(relative_premium, vol, moving_average_perf) then
        return self.weight_value
    else
        return 0
    end
end



--functions exported by package
vanilla_strategy = {
make_implicit_volatility_friction = make_implicit_volatility_friction,
make_adaptative_friction = make_adaptative_friction,
make_price_friction = make_price_friction,
make_friction = make_friction,
make_option = make_option,
make_underlying = make_underlying,
make_portfolio = make_portfolio,
make_delta_bs_spot_position = make_delta_bs_spot_position,
make_delta_bs_converse_position = make_delta_bs_converse_position,
make_delta_skew_spot_position = make_delta_skew_spot_position,
make_delta_skew_converse_position = make_delta_skew_converse_position,
make_delta_converse_position = make_delta_converse_position,
make_trading_constraints = make_trading_constraints}

