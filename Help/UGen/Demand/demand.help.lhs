demand trig reset ugens

Demand results from demand rate ugens.

When there is a trigger at the trig input, a value is demanded from
each ugen in the list and output. The unit generators in the list
should be 'demand' rate.

When there is a trigger at the reset input, the demand rate ugens
in the list are reset.

trig  - Trigger can be any signal. A trigger happens when
        the signal changes from non-positive to positive.

reset - Resets the list of ugens when triggered.
