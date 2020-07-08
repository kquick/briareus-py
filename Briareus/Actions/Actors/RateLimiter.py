from collections import Counter, defaultdict
from datetime import datetime, timedelta
from typing import (Any, Callable, DefaultDict, Dict, Generic, List,
                    NewType, Optional, Tuple, TypeVar, Union)


TargetType = TypeVar('TargetType')
ActionType = TypeVar('ActionType')
ActionTimeType = Tuple[datetime, ActionType]

class RateLimiter(Generic[TargetType, ActionType]):
    """For an action to a set of target, keep track of how often that
       action was performed for each target (for the last 24 hours).
       The initialization can specify a maximum number of actions that
       should actually be performed for each target over an hour and a
       day.  The initialization can also be passed an optional list of
       targets that will not be rate limited if they appear.

       Call allowed() with the action and the list of targets and it
       will return the subset of targets for which the action is
       allowed at this time.

       The status() method will return detailed information about the
       allowed deliveries and the suppressed deliveries over the last
       24 hours, and a daily summary count of both for each target
       over the last 30 days (excluding the detailed entries for the
       last 24 hours).

    """
    def __init__(self,
                 hourly: int = None,  # maximum count per hour (None=unlimited)
                 daily: int = None,   # maximum count per day (None=unlimited)
                 unlimited: List[TargetType] = None) -> None:  # unlimited targets
        self._hourly = hourly
        self._daily = daily
        self._unlimited = unlimited or []

        # _recent is the memory of recent target: [(datetime, actIdent)]
        self._recent: DefaultDict[TargetType, List[ActionTimeType]] = defaultdict(list)
        # _suppressed is like _recent but for suppressed targets
        self._suppressed: DefaultDict[TargetType, List[ActionTimeType]] = defaultdict(list)

        self._allowed_history: DefaultDict[TargetType,List[datetime]] = defaultdict(list)
        self._suppressed_history: DefaultDict[TargetType,List[datetime]] = defaultdict(list)

    def allowed(self, actionIdent: ActionType,
                targets: List[TargetType]) -> List[TargetType]:
        ret = []
        now = datetime.now()
        for each in targets:
            self.bookkeeping_for(now, each)

            if each not in self._unlimited:
                recent = self._recent[each]

                today = list(filter(lambda e: now - e[0] < timedelta(hours=24), recent))
                if self._daily and len(today) >= self._daily:
                    self._suppressed[each].append( (now, actionIdent) )
                    continue

                last_hour = list(filter(lambda e: now - e[0] < timedelta(hours=1), today))
                if self._hourly and len(last_hour) >= self._hourly:
                    self._suppressed[each].append( (now, actionIdent) )
                    continue

            self._recent[each].append( (now, actionIdent) )
            ret.append(each)

        return ret

    def bookkeeping(self) -> None:
        now = datetime.now()
        for each in self._recent:
            self.bookkeeping_for(now, each)

    def bookkeeping_for(self, now: datetime, target: TargetType) -> None:
        self._allowed_history[target] = [e for e in self._allowed_history[target]
                                         if now - e < timedelta(days=30)]
        self._suppressed_history[target] = [e for e in self._suppressed_history[target]
                                            if now - e < timedelta(days=30)]
        self._allowed_history[target].extend([e[0] for e in self._recent[target]
                                              if now - e[0] >= timedelta(hours=24)])
        self._suppressed_history[target].extend([e[0] for e in self._suppressed[target]
                                                 if now - e[0] >= timedelta(hours=24)])
        self._recent[target] = [e for e in self._recent[target]
                                if now - e[0] < timedelta(hours=24)]
        self._suppressed[target] = [e for e in self._suppressed[target]
                                    if now - e[0] < timedelta(hours=24)]

    def status(self) -> Dict[str, Any]:
        self.bookkeeping()

        # Somewhat expensive calculations of the frequencies for the last 30 days
        now = datetime.now()
        month_allowed: DefaultDict[TargetType,Counter] = defaultdict(Counter)
        month_suppressed: DefaultDict[TargetType,Counter] = defaultdict(Counter)
        for each in self._allowed_history:
            for days in range(0, 30):
                on_day = lambda d: ((now - d) >= timedelta(days=days) and
                                    (now - d) < timedelta(days=days+1))
                cnt = len([e for e in self._allowed_history[each] if on_day(e)])
                if cnt:
                    month_allowed[each][days] += cnt
        for each in self._suppressed_history:
            for days in range(0, 30):
                on_day = lambda d: ((now - d) >= timedelta(days=days) and
                                    (now - d) < timedelta(days=days+1))
                cnt = len([e for e in self._suppressed_history[each] if on_day(e)])
                if cnt:
                    month_suppressed[each][days] += cnt

        return {
            'recent' : { 'allowed': self._recent,
                         'suppressed': self._suppressed,
            },
            'this_month' : { 'allowed': month_allowed,
                             'suppressed': month_suppressed,
            },
            'unlimited' : self._unlimited,
            'hourly': self._hourly,
            'daily': self._daily,
        }
